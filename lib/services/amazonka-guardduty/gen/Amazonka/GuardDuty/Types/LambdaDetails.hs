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
-- Module      : Amazonka.GuardDuty.Types.LambdaDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.LambdaDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.Tag
import Amazonka.GuardDuty.Types.VpcConfig
import qualified Amazonka.Prelude as Prelude

-- | Information about the Lambda function involved in the finding.
--
-- /See:/ 'newLambdaDetails' smart constructor.
data LambdaDetails = LambdaDetails'
  { -- | Description of the Lambda function.
    description :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the Lambda function.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | Name of the Lambda function.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The version of the Lambda function.
    functionVersion :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the Lambda function was last modified. This field is
    -- in the UTC date string format @(2023-03-22T19:37:20.168Z)@.
    lastModifiedAt :: Prelude.Maybe Data.POSIX,
    -- | The revision ID of the Lambda function version.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The execution role of the Lambda function.
    role' :: Prelude.Maybe Prelude.Text,
    -- | A list of tags attached to this resource, listed in the format of
    -- @key@:@value@ pair.
    tags :: Prelude.Maybe [Tag],
    -- | Amazon Virtual Private Cloud configuration details associated with your
    -- Lambda function.
    vpcConfig :: Prelude.Maybe VpcConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'lambdaDetails_description' - Description of the Lambda function.
--
-- 'functionArn', 'lambdaDetails_functionArn' - Amazon Resource Name (ARN) of the Lambda function.
--
-- 'functionName', 'lambdaDetails_functionName' - Name of the Lambda function.
--
-- 'functionVersion', 'lambdaDetails_functionVersion' - The version of the Lambda function.
--
-- 'lastModifiedAt', 'lambdaDetails_lastModifiedAt' - The timestamp when the Lambda function was last modified. This field is
-- in the UTC date string format @(2023-03-22T19:37:20.168Z)@.
--
-- 'revisionId', 'lambdaDetails_revisionId' - The revision ID of the Lambda function version.
--
-- 'role'', 'lambdaDetails_role' - The execution role of the Lambda function.
--
-- 'tags', 'lambdaDetails_tags' - A list of tags attached to this resource, listed in the format of
-- @key@:@value@ pair.
--
-- 'vpcConfig', 'lambdaDetails_vpcConfig' - Amazon Virtual Private Cloud configuration details associated with your
-- Lambda function.
newLambdaDetails ::
  LambdaDetails
newLambdaDetails =
  LambdaDetails'
    { description = Prelude.Nothing,
      functionArn = Prelude.Nothing,
      functionName = Prelude.Nothing,
      functionVersion = Prelude.Nothing,
      lastModifiedAt = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      role' = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | Description of the Lambda function.
lambdaDetails_description :: Lens.Lens' LambdaDetails (Prelude.Maybe Prelude.Text)
lambdaDetails_description = Lens.lens (\LambdaDetails' {description} -> description) (\s@LambdaDetails' {} a -> s {description = a} :: LambdaDetails)

-- | Amazon Resource Name (ARN) of the Lambda function.
lambdaDetails_functionArn :: Lens.Lens' LambdaDetails (Prelude.Maybe Prelude.Text)
lambdaDetails_functionArn = Lens.lens (\LambdaDetails' {functionArn} -> functionArn) (\s@LambdaDetails' {} a -> s {functionArn = a} :: LambdaDetails)

-- | Name of the Lambda function.
lambdaDetails_functionName :: Lens.Lens' LambdaDetails (Prelude.Maybe Prelude.Text)
lambdaDetails_functionName = Lens.lens (\LambdaDetails' {functionName} -> functionName) (\s@LambdaDetails' {} a -> s {functionName = a} :: LambdaDetails)

-- | The version of the Lambda function.
lambdaDetails_functionVersion :: Lens.Lens' LambdaDetails (Prelude.Maybe Prelude.Text)
lambdaDetails_functionVersion = Lens.lens (\LambdaDetails' {functionVersion} -> functionVersion) (\s@LambdaDetails' {} a -> s {functionVersion = a} :: LambdaDetails)

-- | The timestamp when the Lambda function was last modified. This field is
-- in the UTC date string format @(2023-03-22T19:37:20.168Z)@.
lambdaDetails_lastModifiedAt :: Lens.Lens' LambdaDetails (Prelude.Maybe Prelude.UTCTime)
lambdaDetails_lastModifiedAt = Lens.lens (\LambdaDetails' {lastModifiedAt} -> lastModifiedAt) (\s@LambdaDetails' {} a -> s {lastModifiedAt = a} :: LambdaDetails) Prelude.. Lens.mapping Data._Time

-- | The revision ID of the Lambda function version.
lambdaDetails_revisionId :: Lens.Lens' LambdaDetails (Prelude.Maybe Prelude.Text)
lambdaDetails_revisionId = Lens.lens (\LambdaDetails' {revisionId} -> revisionId) (\s@LambdaDetails' {} a -> s {revisionId = a} :: LambdaDetails)

-- | The execution role of the Lambda function.
lambdaDetails_role :: Lens.Lens' LambdaDetails (Prelude.Maybe Prelude.Text)
lambdaDetails_role = Lens.lens (\LambdaDetails' {role'} -> role') (\s@LambdaDetails' {} a -> s {role' = a} :: LambdaDetails)

-- | A list of tags attached to this resource, listed in the format of
-- @key@:@value@ pair.
lambdaDetails_tags :: Lens.Lens' LambdaDetails (Prelude.Maybe [Tag])
lambdaDetails_tags = Lens.lens (\LambdaDetails' {tags} -> tags) (\s@LambdaDetails' {} a -> s {tags = a} :: LambdaDetails) Prelude.. Lens.mapping Lens.coerced

-- | Amazon Virtual Private Cloud configuration details associated with your
-- Lambda function.
lambdaDetails_vpcConfig :: Lens.Lens' LambdaDetails (Prelude.Maybe VpcConfig)
lambdaDetails_vpcConfig = Lens.lens (\LambdaDetails' {vpcConfig} -> vpcConfig) (\s@LambdaDetails' {} a -> s {vpcConfig = a} :: LambdaDetails)

instance Data.FromJSON LambdaDetails where
  parseJSON =
    Data.withObject
      "LambdaDetails"
      ( \x ->
          LambdaDetails'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "functionArn")
            Prelude.<*> (x Data..:? "functionName")
            Prelude.<*> (x Data..:? "functionVersion")
            Prelude.<*> (x Data..:? "lastModifiedAt")
            Prelude.<*> (x Data..:? "revisionId")
            Prelude.<*> (x Data..:? "role")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vpcConfig")
      )

instance Prelude.Hashable LambdaDetails where
  hashWithSalt _salt LambdaDetails' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` functionArn
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` functionVersion
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData LambdaDetails where
  rnf LambdaDetails' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf functionVersion
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcConfig
