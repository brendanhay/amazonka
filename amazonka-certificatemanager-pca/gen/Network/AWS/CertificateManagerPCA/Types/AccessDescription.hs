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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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

instance Core.FromJSON AccessDescription where
  parseJSON =
    Core.withObject
      "AccessDescription"
      ( \x ->
          AccessDescription'
            Core.<$> (x Core..: "AccessMethod")
            Core.<*> (x Core..: "AccessLocation")
      )

instance Core.Hashable AccessDescription

instance Core.NFData AccessDescription

instance Core.ToJSON AccessDescription where
  toJSON AccessDescription' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccessMethod" Core..= accessMethod),
            Core.Just ("AccessLocation" Core..= accessLocation)
          ]
      )
