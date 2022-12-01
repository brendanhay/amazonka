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
-- Module      : Amazonka.AppStream.Types.AppBlock
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.AppBlock where

import Amazonka.AppStream.Types.S3Location
import Amazonka.AppStream.Types.ScriptDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an app block.
--
-- App blocks are an Amazon AppStream 2.0 resource that stores the details
-- about the virtual hard disk in an S3 bucket. It also stores the setup
-- script with details about how to mount the virtual hard disk. The
-- virtual hard disk includes the application binaries and other files
-- necessary to launch your applications. Multiple applications can be
-- assigned to a single app block.
--
-- This is only supported for Elastic fleets.
--
-- /See:/ 'newAppBlock' smart constructor.
data AppBlock = AppBlock'
  { -- | The created time of the app block.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The display name of the app block.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The description of the app block.
    description :: Prelude.Maybe Prelude.Text,
    -- | The source S3 location of the app block.
    sourceS3Location :: Prelude.Maybe S3Location,
    -- | The name of the app block.
    name :: Prelude.Text,
    -- | The ARN of the app block.
    arn :: Prelude.Text,
    -- | The setup script details of the app block.
    setupScriptDetails :: ScriptDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTime', 'appBlock_createdTime' - The created time of the app block.
--
-- 'displayName', 'appBlock_displayName' - The display name of the app block.
--
-- 'description', 'appBlock_description' - The description of the app block.
--
-- 'sourceS3Location', 'appBlock_sourceS3Location' - The source S3 location of the app block.
--
-- 'name', 'appBlock_name' - The name of the app block.
--
-- 'arn', 'appBlock_arn' - The ARN of the app block.
--
-- 'setupScriptDetails', 'appBlock_setupScriptDetails' - The setup script details of the app block.
newAppBlock ::
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'setupScriptDetails'
  ScriptDetails ->
  AppBlock
newAppBlock pName_ pArn_ pSetupScriptDetails_ =
  AppBlock'
    { createdTime = Prelude.Nothing,
      displayName = Prelude.Nothing,
      description = Prelude.Nothing,
      sourceS3Location = Prelude.Nothing,
      name = pName_,
      arn = pArn_,
      setupScriptDetails = pSetupScriptDetails_
    }

-- | The created time of the app block.
appBlock_createdTime :: Lens.Lens' AppBlock (Prelude.Maybe Prelude.UTCTime)
appBlock_createdTime = Lens.lens (\AppBlock' {createdTime} -> createdTime) (\s@AppBlock' {} a -> s {createdTime = a} :: AppBlock) Prelude.. Lens.mapping Core._Time

-- | The display name of the app block.
appBlock_displayName :: Lens.Lens' AppBlock (Prelude.Maybe Prelude.Text)
appBlock_displayName = Lens.lens (\AppBlock' {displayName} -> displayName) (\s@AppBlock' {} a -> s {displayName = a} :: AppBlock)

-- | The description of the app block.
appBlock_description :: Lens.Lens' AppBlock (Prelude.Maybe Prelude.Text)
appBlock_description = Lens.lens (\AppBlock' {description} -> description) (\s@AppBlock' {} a -> s {description = a} :: AppBlock)

-- | The source S3 location of the app block.
appBlock_sourceS3Location :: Lens.Lens' AppBlock (Prelude.Maybe S3Location)
appBlock_sourceS3Location = Lens.lens (\AppBlock' {sourceS3Location} -> sourceS3Location) (\s@AppBlock' {} a -> s {sourceS3Location = a} :: AppBlock)

-- | The name of the app block.
appBlock_name :: Lens.Lens' AppBlock Prelude.Text
appBlock_name = Lens.lens (\AppBlock' {name} -> name) (\s@AppBlock' {} a -> s {name = a} :: AppBlock)

-- | The ARN of the app block.
appBlock_arn :: Lens.Lens' AppBlock Prelude.Text
appBlock_arn = Lens.lens (\AppBlock' {arn} -> arn) (\s@AppBlock' {} a -> s {arn = a} :: AppBlock)

-- | The setup script details of the app block.
appBlock_setupScriptDetails :: Lens.Lens' AppBlock ScriptDetails
appBlock_setupScriptDetails = Lens.lens (\AppBlock' {setupScriptDetails} -> setupScriptDetails) (\s@AppBlock' {} a -> s {setupScriptDetails = a} :: AppBlock)

instance Core.FromJSON AppBlock where
  parseJSON =
    Core.withObject
      "AppBlock"
      ( \x ->
          AppBlock'
            Prelude.<$> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "SourceS3Location")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Arn")
            Prelude.<*> (x Core..: "SetupScriptDetails")
      )

instance Prelude.Hashable AppBlock where
  hashWithSalt _salt AppBlock' {..} =
    _salt `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sourceS3Location
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` setupScriptDetails

instance Prelude.NFData AppBlock where
  rnf AppBlock' {..} =
    Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sourceS3Location
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf setupScriptDetails
