{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpgradePublishedSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Upgrades a published schema under a new minor version revision using the
-- current contents of @DevelopmentSchemaArn@.
module Network.AWS.CloudDirectory.UpgradePublishedSchema
  ( -- * Creating a Request
    UpgradePublishedSchema (..),
    newUpgradePublishedSchema,

    -- * Request Lenses
    upgradePublishedSchema_dryRun,
    upgradePublishedSchema_developmentSchemaArn,
    upgradePublishedSchema_publishedSchemaArn,
    upgradePublishedSchema_minorVersion,

    -- * Destructuring the Response
    UpgradePublishedSchemaResponse (..),
    newUpgradePublishedSchemaResponse,

    -- * Response Lenses
    upgradePublishedSchemaResponse_upgradedSchemaArn,
    upgradePublishedSchemaResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpgradePublishedSchema' smart constructor.
data UpgradePublishedSchema = UpgradePublishedSchema'
  { -- | Used for testing whether the Development schema provided is backwards
    -- compatible, or not, with the publish schema provided by the user to be
    -- upgraded. If schema compatibility fails, an exception would be thrown
    -- else the call would succeed. This parameter is optional and defaults to
    -- false.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the development schema with the changes used for the upgrade.
    developmentSchemaArn :: Prelude.Text,
    -- | The ARN of the published schema to be upgraded.
    publishedSchemaArn :: Prelude.Text,
    -- | Identifies the minor version of the published schema that will be
    -- created. This parameter is NOT optional.
    minorVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpgradePublishedSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'upgradePublishedSchema_dryRun' - Used for testing whether the Development schema provided is backwards
-- compatible, or not, with the publish schema provided by the user to be
-- upgraded. If schema compatibility fails, an exception would be thrown
-- else the call would succeed. This parameter is optional and defaults to
-- false.
--
-- 'developmentSchemaArn', 'upgradePublishedSchema_developmentSchemaArn' - The ARN of the development schema with the changes used for the upgrade.
--
-- 'publishedSchemaArn', 'upgradePublishedSchema_publishedSchemaArn' - The ARN of the published schema to be upgraded.
--
-- 'minorVersion', 'upgradePublishedSchema_minorVersion' - Identifies the minor version of the published schema that will be
-- created. This parameter is NOT optional.
newUpgradePublishedSchema ::
  -- | 'developmentSchemaArn'
  Prelude.Text ->
  -- | 'publishedSchemaArn'
  Prelude.Text ->
  -- | 'minorVersion'
  Prelude.Text ->
  UpgradePublishedSchema
newUpgradePublishedSchema
  pDevelopmentSchemaArn_
  pPublishedSchemaArn_
  pMinorVersion_ =
    UpgradePublishedSchema'
      { dryRun = Prelude.Nothing,
        developmentSchemaArn = pDevelopmentSchemaArn_,
        publishedSchemaArn = pPublishedSchemaArn_,
        minorVersion = pMinorVersion_
      }

-- | Used for testing whether the Development schema provided is backwards
-- compatible, or not, with the publish schema provided by the user to be
-- upgraded. If schema compatibility fails, an exception would be thrown
-- else the call would succeed. This parameter is optional and defaults to
-- false.
upgradePublishedSchema_dryRun :: Lens.Lens' UpgradePublishedSchema (Prelude.Maybe Prelude.Bool)
upgradePublishedSchema_dryRun = Lens.lens (\UpgradePublishedSchema' {dryRun} -> dryRun) (\s@UpgradePublishedSchema' {} a -> s {dryRun = a} :: UpgradePublishedSchema)

-- | The ARN of the development schema with the changes used for the upgrade.
upgradePublishedSchema_developmentSchemaArn :: Lens.Lens' UpgradePublishedSchema Prelude.Text
upgradePublishedSchema_developmentSchemaArn = Lens.lens (\UpgradePublishedSchema' {developmentSchemaArn} -> developmentSchemaArn) (\s@UpgradePublishedSchema' {} a -> s {developmentSchemaArn = a} :: UpgradePublishedSchema)

-- | The ARN of the published schema to be upgraded.
upgradePublishedSchema_publishedSchemaArn :: Lens.Lens' UpgradePublishedSchema Prelude.Text
upgradePublishedSchema_publishedSchemaArn = Lens.lens (\UpgradePublishedSchema' {publishedSchemaArn} -> publishedSchemaArn) (\s@UpgradePublishedSchema' {} a -> s {publishedSchemaArn = a} :: UpgradePublishedSchema)

-- | Identifies the minor version of the published schema that will be
-- created. This parameter is NOT optional.
upgradePublishedSchema_minorVersion :: Lens.Lens' UpgradePublishedSchema Prelude.Text
upgradePublishedSchema_minorVersion = Lens.lens (\UpgradePublishedSchema' {minorVersion} -> minorVersion) (\s@UpgradePublishedSchema' {} a -> s {minorVersion = a} :: UpgradePublishedSchema)

instance Prelude.AWSRequest UpgradePublishedSchema where
  type
    Rs UpgradePublishedSchema =
      UpgradePublishedSchemaResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpgradePublishedSchemaResponse'
            Prelude.<$> (x Prelude..?> "UpgradedSchemaArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpgradePublishedSchema

instance Prelude.NFData UpgradePublishedSchema

instance Prelude.ToHeaders UpgradePublishedSchema where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpgradePublishedSchema where
  toJSON UpgradePublishedSchema' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DryRun" Prelude..=) Prelude.<$> dryRun,
            Prelude.Just
              ( "DevelopmentSchemaArn"
                  Prelude..= developmentSchemaArn
              ),
            Prelude.Just
              ("PublishedSchemaArn" Prelude..= publishedSchemaArn),
            Prelude.Just
              ("MinorVersion" Prelude..= minorVersion)
          ]
      )

instance Prelude.ToPath UpgradePublishedSchema where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/upgradepublished"

instance Prelude.ToQuery UpgradePublishedSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpgradePublishedSchemaResponse' smart constructor.
data UpgradePublishedSchemaResponse = UpgradePublishedSchemaResponse'
  { -- | The ARN of the upgraded schema that is returned as part of the response.
    upgradedSchemaArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpgradePublishedSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upgradedSchemaArn', 'upgradePublishedSchemaResponse_upgradedSchemaArn' - The ARN of the upgraded schema that is returned as part of the response.
--
-- 'httpStatus', 'upgradePublishedSchemaResponse_httpStatus' - The response's http status code.
newUpgradePublishedSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpgradePublishedSchemaResponse
newUpgradePublishedSchemaResponse pHttpStatus_ =
  UpgradePublishedSchemaResponse'
    { upgradedSchemaArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the upgraded schema that is returned as part of the response.
upgradePublishedSchemaResponse_upgradedSchemaArn :: Lens.Lens' UpgradePublishedSchemaResponse (Prelude.Maybe Prelude.Text)
upgradePublishedSchemaResponse_upgradedSchemaArn = Lens.lens (\UpgradePublishedSchemaResponse' {upgradedSchemaArn} -> upgradedSchemaArn) (\s@UpgradePublishedSchemaResponse' {} a -> s {upgradedSchemaArn = a} :: UpgradePublishedSchemaResponse)

-- | The response's http status code.
upgradePublishedSchemaResponse_httpStatus :: Lens.Lens' UpgradePublishedSchemaResponse Prelude.Int
upgradePublishedSchemaResponse_httpStatus = Lens.lens (\UpgradePublishedSchemaResponse' {httpStatus} -> httpStatus) (\s@UpgradePublishedSchemaResponse' {} a -> s {httpStatus = a} :: UpgradePublishedSchemaResponse)

instance
  Prelude.NFData
    UpgradePublishedSchemaResponse
