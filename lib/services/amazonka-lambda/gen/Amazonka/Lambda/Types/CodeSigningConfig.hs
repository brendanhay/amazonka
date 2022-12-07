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
-- Module      : Amazonka.Lambda.Types.CodeSigningConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.CodeSigningConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.AllowedPublishers
import Amazonka.Lambda.Types.CodeSigningPolicies
import qualified Amazonka.Prelude as Prelude

-- | Details about a
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-codesigning.html Code signing configuration>.
--
-- /See:/ 'newCodeSigningConfig' smart constructor.
data CodeSigningConfig = CodeSigningConfig'
  { -- | Code signing configuration description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Unique identifer for the Code signing configuration.
    codeSigningConfigId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Code signing configuration.
    codeSigningConfigArn :: Prelude.Text,
    -- | List of allowed publishers.
    allowedPublishers :: AllowedPublishers,
    -- | The code signing policy controls the validation failure action for
    -- signature mismatch or expiry.
    codeSigningPolicies :: CodeSigningPolicies,
    -- | The date and time that the Code signing configuration was last modified,
    -- in ISO-8601 format (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModified :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'codeSigningConfigArn'
  Prelude.Text ->
  -- | 'allowedPublishers'
  AllowedPublishers ->
  -- | 'codeSigningPolicies'
  CodeSigningPolicies ->
  -- | 'lastModified'
  Prelude.Text ->
  CodeSigningConfig
newCodeSigningConfig
  pCodeSigningConfigId_
  pCodeSigningConfigArn_
  pAllowedPublishers_
  pCodeSigningPolicies_
  pLastModified_ =
    CodeSigningConfig'
      { description = Prelude.Nothing,
        codeSigningConfigId = pCodeSigningConfigId_,
        codeSigningConfigArn = pCodeSigningConfigArn_,
        allowedPublishers = pAllowedPublishers_,
        codeSigningPolicies = pCodeSigningPolicies_,
        lastModified = pLastModified_
      }

-- | Code signing configuration description.
codeSigningConfig_description :: Lens.Lens' CodeSigningConfig (Prelude.Maybe Prelude.Text)
codeSigningConfig_description = Lens.lens (\CodeSigningConfig' {description} -> description) (\s@CodeSigningConfig' {} a -> s {description = a} :: CodeSigningConfig)

-- | Unique identifer for the Code signing configuration.
codeSigningConfig_codeSigningConfigId :: Lens.Lens' CodeSigningConfig Prelude.Text
codeSigningConfig_codeSigningConfigId = Lens.lens (\CodeSigningConfig' {codeSigningConfigId} -> codeSigningConfigId) (\s@CodeSigningConfig' {} a -> s {codeSigningConfigId = a} :: CodeSigningConfig)

-- | The Amazon Resource Name (ARN) of the Code signing configuration.
codeSigningConfig_codeSigningConfigArn :: Lens.Lens' CodeSigningConfig Prelude.Text
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
codeSigningConfig_lastModified :: Lens.Lens' CodeSigningConfig Prelude.Text
codeSigningConfig_lastModified = Lens.lens (\CodeSigningConfig' {lastModified} -> lastModified) (\s@CodeSigningConfig' {} a -> s {lastModified = a} :: CodeSigningConfig)

instance Data.FromJSON CodeSigningConfig where
  parseJSON =
    Data.withObject
      "CodeSigningConfig"
      ( \x ->
          CodeSigningConfig'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..: "CodeSigningConfigId")
            Prelude.<*> (x Data..: "CodeSigningConfigArn")
            Prelude.<*> (x Data..: "AllowedPublishers")
            Prelude.<*> (x Data..: "CodeSigningPolicies")
            Prelude.<*> (x Data..: "LastModified")
      )

instance Prelude.Hashable CodeSigningConfig where
  hashWithSalt _salt CodeSigningConfig' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` codeSigningConfigId
      `Prelude.hashWithSalt` codeSigningConfigArn
      `Prelude.hashWithSalt` allowedPublishers
      `Prelude.hashWithSalt` codeSigningPolicies
      `Prelude.hashWithSalt` lastModified

instance Prelude.NFData CodeSigningConfig where
  rnf CodeSigningConfig' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf codeSigningConfigId
      `Prelude.seq` Prelude.rnf codeSigningConfigArn
      `Prelude.seq` Prelude.rnf allowedPublishers
      `Prelude.seq` Prelude.rnf codeSigningPolicies
      `Prelude.seq` Prelude.rnf lastModified
