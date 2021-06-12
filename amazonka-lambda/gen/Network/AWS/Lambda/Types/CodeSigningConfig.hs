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
-- Module      : Network.AWS.Lambda.Types.CodeSigningConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.CodeSigningConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.AllowedPublishers
import Network.AWS.Lambda.Types.CodeSigningPolicies
import qualified Network.AWS.Lens as Lens

-- | Details about a Code signing configuration.
--
-- /See:/ 'newCodeSigningConfig' smart constructor.
data CodeSigningConfig = CodeSigningConfig'
  { -- | Code signing configuration description.
    description :: Core.Maybe Core.Text,
    -- | Unique identifer for the Code signing configuration.
    codeSigningConfigId :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the Code signing configuration.
    codeSigningConfigArn :: Core.Text,
    -- | List of allowed publishers.
    allowedPublishers :: AllowedPublishers,
    -- | The code signing policy controls the validation failure action for
    -- signature mismatch or expiry.
    codeSigningPolicies :: CodeSigningPolicies,
    -- | The date and time that the Code signing configuration was last modified,
    -- in ISO-8601 format (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModified :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CodeSigningConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'codeSigningConfig_description' - Code signing configuration description.
--
-- 'codeSigningConfigId', 'codeSigningConfig_codeSigningConfigId' - Unique identifer for the Code signing configuration.
--
-- 'codeSigningConfigArn', 'codeSigningConfig_codeSigningConfigArn' - The Amazon Resource Name (ARN) of the Code signing configuration.
--
-- 'allowedPublishers', 'codeSigningConfig_allowedPublishers' - List of allowed publishers.
--
-- 'codeSigningPolicies', 'codeSigningConfig_codeSigningPolicies' - The code signing policy controls the validation failure action for
-- signature mismatch or expiry.
--
-- 'lastModified', 'codeSigningConfig_lastModified' - The date and time that the Code signing configuration was last modified,
-- in ISO-8601 format (YYYY-MM-DDThh:mm:ss.sTZD).
newCodeSigningConfig ::
  -- | 'codeSigningConfigId'
  Core.Text ->
  -- | 'codeSigningConfigArn'
  Core.Text ->
  -- | 'allowedPublishers'
  AllowedPublishers ->
  -- | 'codeSigningPolicies'
  CodeSigningPolicies ->
  -- | 'lastModified'
  Core.Text ->
  CodeSigningConfig
newCodeSigningConfig
  pCodeSigningConfigId_
  pCodeSigningConfigArn_
  pAllowedPublishers_
  pCodeSigningPolicies_
  pLastModified_ =
    CodeSigningConfig'
      { description = Core.Nothing,
        codeSigningConfigId = pCodeSigningConfigId_,
        codeSigningConfigArn = pCodeSigningConfigArn_,
        allowedPublishers = pAllowedPublishers_,
        codeSigningPolicies = pCodeSigningPolicies_,
        lastModified = pLastModified_
      }

-- | Code signing configuration description.
codeSigningConfig_description :: Lens.Lens' CodeSigningConfig (Core.Maybe Core.Text)
codeSigningConfig_description = Lens.lens (\CodeSigningConfig' {description} -> description) (\s@CodeSigningConfig' {} a -> s {description = a} :: CodeSigningConfig)

-- | Unique identifer for the Code signing configuration.
codeSigningConfig_codeSigningConfigId :: Lens.Lens' CodeSigningConfig Core.Text
codeSigningConfig_codeSigningConfigId = Lens.lens (\CodeSigningConfig' {codeSigningConfigId} -> codeSigningConfigId) (\s@CodeSigningConfig' {} a -> s {codeSigningConfigId = a} :: CodeSigningConfig)

-- | The Amazon Resource Name (ARN) of the Code signing configuration.
codeSigningConfig_codeSigningConfigArn :: Lens.Lens' CodeSigningConfig Core.Text
codeSigningConfig_codeSigningConfigArn = Lens.lens (\CodeSigningConfig' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@CodeSigningConfig' {} a -> s {codeSigningConfigArn = a} :: CodeSigningConfig)

-- | List of allowed publishers.
codeSigningConfig_allowedPublishers :: Lens.Lens' CodeSigningConfig AllowedPublishers
codeSigningConfig_allowedPublishers = Lens.lens (\CodeSigningConfig' {allowedPublishers} -> allowedPublishers) (\s@CodeSigningConfig' {} a -> s {allowedPublishers = a} :: CodeSigningConfig)

-- | The code signing policy controls the validation failure action for
-- signature mismatch or expiry.
codeSigningConfig_codeSigningPolicies :: Lens.Lens' CodeSigningConfig CodeSigningPolicies
codeSigningConfig_codeSigningPolicies = Lens.lens (\CodeSigningConfig' {codeSigningPolicies} -> codeSigningPolicies) (\s@CodeSigningConfig' {} a -> s {codeSigningPolicies = a} :: CodeSigningConfig)

-- | The date and time that the Code signing configuration was last modified,
-- in ISO-8601 format (YYYY-MM-DDThh:mm:ss.sTZD).
codeSigningConfig_lastModified :: Lens.Lens' CodeSigningConfig Core.Text
codeSigningConfig_lastModified = Lens.lens (\CodeSigningConfig' {lastModified} -> lastModified) (\s@CodeSigningConfig' {} a -> s {lastModified = a} :: CodeSigningConfig)

instance Core.FromJSON CodeSigningConfig where
  parseJSON =
    Core.withObject
      "CodeSigningConfig"
      ( \x ->
          CodeSigningConfig'
            Core.<$> (x Core..:? "Description")
            Core.<*> (x Core..: "CodeSigningConfigId")
            Core.<*> (x Core..: "CodeSigningConfigArn")
            Core.<*> (x Core..: "AllowedPublishers")
            Core.<*> (x Core..: "CodeSigningPolicies")
            Core.<*> (x Core..: "LastModified")
      )

instance Core.Hashable CodeSigningConfig

instance Core.NFData CodeSigningConfig
