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
-- Module      : Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | ESAM ManifestConfirmConditionNotification defined by
-- OC-SP-ESAM-API-I03-131025.
--
-- /See:/ 'newEsamManifestConfirmConditionNotification' smart constructor.
data EsamManifestConfirmConditionNotification = EsamManifestConfirmConditionNotification'
  { -- | Provide your ESAM ManifestConfirmConditionNotification XML document
    -- inside your JSON job settings. Form the XML document as per
    -- OC-SP-ESAM-API-I03-131025. The transcoder will use the Manifest
    -- Conditioning instructions in the message that you supply.
    mccXml :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EsamManifestConfirmConditionNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mccXml', 'esamManifestConfirmConditionNotification_mccXml' - Provide your ESAM ManifestConfirmConditionNotification XML document
-- inside your JSON job settings. Form the XML document as per
-- OC-SP-ESAM-API-I03-131025. The transcoder will use the Manifest
-- Conditioning instructions in the message that you supply.
newEsamManifestConfirmConditionNotification ::
  EsamManifestConfirmConditionNotification
newEsamManifestConfirmConditionNotification =
  EsamManifestConfirmConditionNotification'
    { mccXml =
        Core.Nothing
    }

-- | Provide your ESAM ManifestConfirmConditionNotification XML document
-- inside your JSON job settings. Form the XML document as per
-- OC-SP-ESAM-API-I03-131025. The transcoder will use the Manifest
-- Conditioning instructions in the message that you supply.
esamManifestConfirmConditionNotification_mccXml :: Lens.Lens' EsamManifestConfirmConditionNotification (Core.Maybe Core.Text)
esamManifestConfirmConditionNotification_mccXml = Lens.lens (\EsamManifestConfirmConditionNotification' {mccXml} -> mccXml) (\s@EsamManifestConfirmConditionNotification' {} a -> s {mccXml = a} :: EsamManifestConfirmConditionNotification)

instance
  Core.FromJSON
    EsamManifestConfirmConditionNotification
  where
  parseJSON =
    Core.withObject
      "EsamManifestConfirmConditionNotification"
      ( \x ->
          EsamManifestConfirmConditionNotification'
            Core.<$> (x Core..:? "mccXml")
      )

instance
  Core.Hashable
    EsamManifestConfirmConditionNotification

instance
  Core.NFData
    EsamManifestConfirmConditionNotification

instance
  Core.ToJSON
    EsamManifestConfirmConditionNotification
  where
  toJSON EsamManifestConfirmConditionNotification' {..} =
    Core.object
      (Core.catMaybes [("mccXml" Core..=) Core.<$> mccXml])
