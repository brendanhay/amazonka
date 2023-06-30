{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CertificateManagerPCA.Types.AccessDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.AccessDescription where

import Amazonka.CertificateManagerPCA.Types.AccessMethod
import Amazonka.CertificateManagerPCA.Types.GeneralName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides access information used by the @authorityInfoAccess@ and
-- @subjectInfoAccess@ extensions described in
-- <https://datatracker.ietf.org/doc/html/rfc5280 RFC 5280>.
--
-- /See:/ 'newAccessDescription' smart constructor.
data AccessDescription = AccessDescription'
  { -- | The type and format of @AccessDescription@ information.
    accessMethod :: AccessMethod,
    -- | The location of @AccessDescription@ information.
    accessLocation :: GeneralName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessMethod', 'accessDescription_accessMethod' - The type and format of @AccessDescription@ information.
--
-- 'accessLocation', 'accessDescription_accessLocation' - The location of @AccessDescription@ information.
newAccessDescription ::
  -- | 'accessMethod'
  AccessMethod ->
  -- | 'accessLocation'
  GeneralName ->
  AccessDescription
newAccessDescription pAccessMethod_ pAccessLocation_ =
  AccessDescription'
    { accessMethod = pAccessMethod_,
      accessLocation = pAccessLocation_
    }

-- | The type and format of @AccessDescription@ information.
accessDescription_accessMethod :: Lens.Lens' AccessDescription AccessMethod
accessDescription_accessMethod = Lens.lens (\AccessDescription' {accessMethod} -> accessMethod) (\s@AccessDescription' {} a -> s {accessMethod = a} :: AccessDescription)

-- | The location of @AccessDescription@ information.
accessDescription_accessLocation :: Lens.Lens' AccessDescription GeneralName
accessDescription_accessLocation = Lens.lens (\AccessDescription' {accessLocation} -> accessLocation) (\s@AccessDescription' {} a -> s {accessLocation = a} :: AccessDescription)

instance Data.FromJSON AccessDescription where
  parseJSON =
    Data.withObject
      "AccessDescription"
      ( \x ->
          AccessDescription'
            Prelude.<$> (x Data..: "AccessMethod")
            Prelude.<*> (x Data..: "AccessLocation")
      )

instance Prelude.Hashable AccessDescription where
  hashWithSalt _salt AccessDescription' {..} =
    _salt
      `Prelude.hashWithSalt` accessMethod
      `Prelude.hashWithSalt` accessLocation

instance Prelude.NFData AccessDescription where
  rnf AccessDescription' {..} =
    Prelude.rnf accessMethod
      `Prelude.seq` Prelude.rnf accessLocation

instance Data.ToJSON AccessDescription where
  toJSON AccessDescription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccessMethod" Data..= accessMethod),
            Prelude.Just
              ("AccessLocation" Data..= accessLocation)
          ]
      )
