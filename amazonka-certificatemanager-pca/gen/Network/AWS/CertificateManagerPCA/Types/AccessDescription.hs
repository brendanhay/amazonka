{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CertificateManagerPCA.Types.AccessDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.AccessDescription where

import Network.AWS.CertificateManagerPCA.Types.AccessMethod
import Network.AWS.CertificateManagerPCA.Types.GeneralName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides access information used by the @authorityInfoAccess@ and
-- @subjectInfoAccess@ extensions described in
-- <https://tools.ietf.org/html/rfc5280 RFC 5280>.
--
-- /See:/ 'newAccessDescription' smart constructor.
data AccessDescription = AccessDescription'
  { -- | The type and format of @AccessDescription@ information.
    accessMethod :: AccessMethod,
    -- | The location of @AccessDescription@ information.
    accessLocation :: GeneralName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON AccessDescription where
  parseJSON =
    Prelude.withObject
      "AccessDescription"
      ( \x ->
          AccessDescription'
            Prelude.<$> (x Prelude..: "AccessMethod")
            Prelude.<*> (x Prelude..: "AccessLocation")
      )

instance Prelude.Hashable AccessDescription

instance Prelude.NFData AccessDescription

instance Prelude.ToJSON AccessDescription where
  toJSON AccessDescription' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AccessMethod" Prelude..= accessMethod),
            Prelude.Just
              ("AccessLocation" Prelude..= accessLocation)
          ]
      )
