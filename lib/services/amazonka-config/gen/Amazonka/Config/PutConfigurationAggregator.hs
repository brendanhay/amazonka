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
-- Module      : Amazonka.Config.PutConfigurationAggregator
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- @DescribeConfigurationAggregators@ to get the previous accounts and then
-- append new ones.
--
-- Config should be enabled in source accounts and regions you want to
-- aggregate.
--
-- If your source type is an organization, you must be signed in to the
-- management account or a registered delegated administrator and all the
-- features must be enabled in your organization. If the caller is a
-- management account, Config calls @EnableAwsServiceAccess@ API to enable
-- integration between Config and Organizations. If the caller is a
-- registered delegated administrator, Config calls
-- @ListDelegatedAdministrators@ API to verify whether the caller is a
-- valid delegated administrator.
--
-- To register a delegated administrator, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/set-up-aggregator-cli.html#register-a-delegated-administrator-cli Register a Delegated Administrator>
-- in the /Config developer guide/.
module Amazonka.Config.PutConfigurationAggregator
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutConfigurationAggregator' smart constructor.
data PutConfigurationAggregator = PutConfigurationAggregator'
  { -- | An array of tag object.
    tags :: Prelude.Maybe [Tag],
    -- | A list of AccountAggregationSource object.
    accountAggregationSources :: Prelude.Maybe [AccountAggregationSource],
    -- | An OrganizationAggregationSource object.
    organizationAggregationSource :: Prelude.Maybe OrganizationAggregationSource,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  PutConfigurationAggregator
newPutConfigurationAggregator
  pConfigurationAggregatorName_ =
    PutConfigurationAggregator'
      { tags = Prelude.Nothing,
        accountAggregationSources = Prelude.Nothing,
        organizationAggregationSource = Prelude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | An array of tag object.
putConfigurationAggregator_tags :: Lens.Lens' PutConfigurationAggregator (Prelude.Maybe [Tag])
putConfigurationAggregator_tags = Lens.lens (\PutConfigurationAggregator' {tags} -> tags) (\s@PutConfigurationAggregator' {} a -> s {tags = a} :: PutConfigurationAggregator) Prelude.. Lens.mapping Lens.coerced

-- | A list of AccountAggregationSource object.
putConfigurationAggregator_accountAggregationSources :: Lens.Lens' PutConfigurationAggregator (Prelude.Maybe [AccountAggregationSource])
putConfigurationAggregator_accountAggregationSources = Lens.lens (\PutConfigurationAggregator' {accountAggregationSources} -> accountAggregationSources) (\s@PutConfigurationAggregator' {} a -> s {accountAggregationSources = a} :: PutConfigurationAggregator) Prelude.. Lens.mapping Lens.coerced

-- | An OrganizationAggregationSource object.
putConfigurationAggregator_organizationAggregationSource :: Lens.Lens' PutConfigurationAggregator (Prelude.Maybe OrganizationAggregationSource)
putConfigurationAggregator_organizationAggregationSource = Lens.lens (\PutConfigurationAggregator' {organizationAggregationSource} -> organizationAggregationSource) (\s@PutConfigurationAggregator' {} a -> s {organizationAggregationSource = a} :: PutConfigurationAggregator)

-- | The name of the configuration aggregator.
putConfigurationAggregator_configurationAggregatorName :: Lens.Lens' PutConfigurationAggregator Prelude.Text
putConfigurationAggregator_configurationAggregatorName = Lens.lens (\PutConfigurationAggregator' {configurationAggregatorName} -> configurationAggregatorName) (\s@PutConfigurationAggregator' {} a -> s {configurationAggregatorName = a} :: PutConfigurationAggregator)

instance Core.AWSRequest PutConfigurationAggregator where
  type
    AWSResponse PutConfigurationAggregator =
      PutConfigurationAggregatorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutConfigurationAggregatorResponse'
            Prelude.<$> (x Data..?> "ConfigurationAggregator")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutConfigurationAggregator where
  hashWithSalt _salt PutConfigurationAggregator' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` accountAggregationSources
      `Prelude.hashWithSalt` organizationAggregationSource
      `Prelude.hashWithSalt` configurationAggregatorName

instance Prelude.NFData PutConfigurationAggregator where
  rnf PutConfigurationAggregator' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf accountAggregationSources
      `Prelude.seq` Prelude.rnf organizationAggregationSource
      `Prelude.seq` Prelude.rnf configurationAggregatorName

instance Data.ToHeaders PutConfigurationAggregator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.PutConfigurationAggregator" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutConfigurationAggregator where
  toJSON PutConfigurationAggregator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("AccountAggregationSources" Data..=)
              Prelude.<$> accountAggregationSources,
            ("OrganizationAggregationSource" Data..=)
              Prelude.<$> organizationAggregationSource,
            Prelude.Just
              ( "ConfigurationAggregatorName"
                  Data..= configurationAggregatorName
              )
          ]
      )

instance Data.ToPath PutConfigurationAggregator where
  toPath = Prelude.const "/"

instance Data.ToQuery PutConfigurationAggregator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutConfigurationAggregatorResponse' smart constructor.
data PutConfigurationAggregatorResponse = PutConfigurationAggregatorResponse'
  { -- | Returns a ConfigurationAggregator object.
    configurationAggregator :: Prelude.Maybe ConfigurationAggregator,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutConfigurationAggregatorResponse
newPutConfigurationAggregatorResponse pHttpStatus_ =
  PutConfigurationAggregatorResponse'
    { configurationAggregator =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a ConfigurationAggregator object.
putConfigurationAggregatorResponse_configurationAggregator :: Lens.Lens' PutConfigurationAggregatorResponse (Prelude.Maybe ConfigurationAggregator)
putConfigurationAggregatorResponse_configurationAggregator = Lens.lens (\PutConfigurationAggregatorResponse' {configurationAggregator} -> configurationAggregator) (\s@PutConfigurationAggregatorResponse' {} a -> s {configurationAggregator = a} :: PutConfigurationAggregatorResponse)

-- | The response's http status code.
putConfigurationAggregatorResponse_httpStatus :: Lens.Lens' PutConfigurationAggregatorResponse Prelude.Int
putConfigurationAggregatorResponse_httpStatus = Lens.lens (\PutConfigurationAggregatorResponse' {httpStatus} -> httpStatus) (\s@PutConfigurationAggregatorResponse' {} a -> s {httpStatus = a} :: PutConfigurationAggregatorResponse)

instance
  Prelude.NFData
    PutConfigurationAggregatorResponse
  where
  rnf PutConfigurationAggregatorResponse' {..} =
    Prelude.rnf configurationAggregator
      `Prelude.seq` Prelude.rnf httpStatus
