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
-- Module      : Network.AWS.Config.PutConfigurationAggregator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and updates the configuration aggregator with the selected
-- source accounts and regions. The source account can be individual
-- account(s) or an organization.
--
-- @accountIds@ that are passed will be replaced with existing accounts. If
-- you want to add additional accounts into the aggregator, call
-- @DescribeAggregator@ to get the previous accounts and then append new
-- ones.
--
-- AWS Config should be enabled in source accounts and regions you want to
-- aggregate.
--
-- If your source type is an organization, you must be signed in to the
-- management account or a registered delegated administrator and all the
-- features must be enabled in your organization. If the caller is a
-- management account, AWS Config calls @EnableAwsServiceAccess@ API to
-- enable integration between AWS Config and AWS Organizations. If the
-- caller is a registered delegated administrator, AWS Config calls
-- @ListDelegatedAdministrators@ API to verify whether the caller is a
-- valid delegated administrator.
--
-- To register a delegated administrator, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/set-up-aggregator-cli.html#register-a-delegated-administrator-cli Register a Delegated Administrator>
-- in the AWS Config developer guide.
module Network.AWS.Config.PutConfigurationAggregator
  ( -- * Creating a Request
    PutConfigurationAggregator (..),
    newPutConfigurationAggregator,

    -- * Request Lenses
    putConfigurationAggregator_tags,
    putConfigurationAggregator_accountAggregationSources,
    putConfigurationAggregator_organizationAggregationSource,
    putConfigurationAggregator_configurationAggregatorName,

    -- * Destructuring the Response
    PutConfigurationAggregatorResponse (..),
    newPutConfigurationAggregatorResponse,

    -- * Response Lenses
    putConfigurationAggregatorResponse_configurationAggregator,
    putConfigurationAggregatorResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutConfigurationAggregator' smart constructor.
data PutConfigurationAggregator = PutConfigurationAggregator'
  { -- | An array of tag object.
    tags :: Core.Maybe [Tag],
    -- | A list of AccountAggregationSource object.
    accountAggregationSources :: Core.Maybe [AccountAggregationSource],
    -- | An OrganizationAggregationSource object.
    organizationAggregationSource :: Core.Maybe OrganizationAggregationSource,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutConfigurationAggregator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'putConfigurationAggregator_tags' - An array of tag object.
--
-- 'accountAggregationSources', 'putConfigurationAggregator_accountAggregationSources' - A list of AccountAggregationSource object.
--
-- 'organizationAggregationSource', 'putConfigurationAggregator_organizationAggregationSource' - An OrganizationAggregationSource object.
--
-- 'configurationAggregatorName', 'putConfigurationAggregator_configurationAggregatorName' - The name of the configuration aggregator.
newPutConfigurationAggregator ::
  -- | 'configurationAggregatorName'
  Core.Text ->
  PutConfigurationAggregator
newPutConfigurationAggregator
  pConfigurationAggregatorName_ =
    PutConfigurationAggregator'
      { tags = Core.Nothing,
        accountAggregationSources = Core.Nothing,
        organizationAggregationSource = Core.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | An array of tag object.
putConfigurationAggregator_tags :: Lens.Lens' PutConfigurationAggregator (Core.Maybe [Tag])
putConfigurationAggregator_tags = Lens.lens (\PutConfigurationAggregator' {tags} -> tags) (\s@PutConfigurationAggregator' {} a -> s {tags = a} :: PutConfigurationAggregator) Core.. Lens.mapping Lens._Coerce

-- | A list of AccountAggregationSource object.
putConfigurationAggregator_accountAggregationSources :: Lens.Lens' PutConfigurationAggregator (Core.Maybe [AccountAggregationSource])
putConfigurationAggregator_accountAggregationSources = Lens.lens (\PutConfigurationAggregator' {accountAggregationSources} -> accountAggregationSources) (\s@PutConfigurationAggregator' {} a -> s {accountAggregationSources = a} :: PutConfigurationAggregator) Core.. Lens.mapping Lens._Coerce

-- | An OrganizationAggregationSource object.
putConfigurationAggregator_organizationAggregationSource :: Lens.Lens' PutConfigurationAggregator (Core.Maybe OrganizationAggregationSource)
putConfigurationAggregator_organizationAggregationSource = Lens.lens (\PutConfigurationAggregator' {organizationAggregationSource} -> organizationAggregationSource) (\s@PutConfigurationAggregator' {} a -> s {organizationAggregationSource = a} :: PutConfigurationAggregator)

-- | The name of the configuration aggregator.
putConfigurationAggregator_configurationAggregatorName :: Lens.Lens' PutConfigurationAggregator Core.Text
putConfigurationAggregator_configurationAggregatorName = Lens.lens (\PutConfigurationAggregator' {configurationAggregatorName} -> configurationAggregatorName) (\s@PutConfigurationAggregator' {} a -> s {configurationAggregatorName = a} :: PutConfigurationAggregator)

instance Core.AWSRequest PutConfigurationAggregator where
  type
    AWSResponse PutConfigurationAggregator =
      PutConfigurationAggregatorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutConfigurationAggregatorResponse'
            Core.<$> (x Core..?> "ConfigurationAggregator")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutConfigurationAggregator

instance Core.NFData PutConfigurationAggregator

instance Core.ToHeaders PutConfigurationAggregator where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.PutConfigurationAggregator" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutConfigurationAggregator where
  toJSON PutConfigurationAggregator' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("AccountAggregationSources" Core..=)
              Core.<$> accountAggregationSources,
            ("OrganizationAggregationSource" Core..=)
              Core.<$> organizationAggregationSource,
            Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              )
          ]
      )

instance Core.ToPath PutConfigurationAggregator where
  toPath = Core.const "/"

instance Core.ToQuery PutConfigurationAggregator where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutConfigurationAggregatorResponse' smart constructor.
data PutConfigurationAggregatorResponse = PutConfigurationAggregatorResponse'
  { -- | Returns a ConfigurationAggregator object.
    configurationAggregator :: Core.Maybe ConfigurationAggregator,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutConfigurationAggregatorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationAggregator', 'putConfigurationAggregatorResponse_configurationAggregator' - Returns a ConfigurationAggregator object.
--
-- 'httpStatus', 'putConfigurationAggregatorResponse_httpStatus' - The response's http status code.
newPutConfigurationAggregatorResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutConfigurationAggregatorResponse
newPutConfigurationAggregatorResponse pHttpStatus_ =
  PutConfigurationAggregatorResponse'
    { configurationAggregator =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a ConfigurationAggregator object.
putConfigurationAggregatorResponse_configurationAggregator :: Lens.Lens' PutConfigurationAggregatorResponse (Core.Maybe ConfigurationAggregator)
putConfigurationAggregatorResponse_configurationAggregator = Lens.lens (\PutConfigurationAggregatorResponse' {configurationAggregator} -> configurationAggregator) (\s@PutConfigurationAggregatorResponse' {} a -> s {configurationAggregator = a} :: PutConfigurationAggregatorResponse)

-- | The response's http status code.
putConfigurationAggregatorResponse_httpStatus :: Lens.Lens' PutConfigurationAggregatorResponse Core.Int
putConfigurationAggregatorResponse_httpStatus = Lens.lens (\PutConfigurationAggregatorResponse' {httpStatus} -> httpStatus) (\s@PutConfigurationAggregatorResponse' {} a -> s {httpStatus = a} :: PutConfigurationAggregatorResponse)

instance
  Core.NFData
    PutConfigurationAggregatorResponse
