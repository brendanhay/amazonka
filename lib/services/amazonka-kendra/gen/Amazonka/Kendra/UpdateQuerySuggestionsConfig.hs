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
-- Module      : Amazonka.Kendra.UpdateQuerySuggestionsConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of query suggestions for an index.
--
-- Amazon Kendra supports partial updates, so you only need to provide the
-- fields you want to update.
--
-- If an update is currently processing, you need to wait for the update to
-- finish before making another update.
--
-- Updates to query suggestions settings might not take effect right away.
-- The time for your updated settings to take effect depends on the updates
-- made and the number of search queries in your index.
--
-- You can still enable\/disable query suggestions at any time.
--
-- @UpdateQuerySuggestionsConfig@ is currently not supported in the Amazon
-- Web Services GovCloud (US-West) region.
module Amazonka.Kendra.UpdateQuerySuggestionsConfig
  ( -- * Creating a Request
    UpdateQuerySuggestionsConfig (..),
    newUpdateQuerySuggestionsConfig,

    -- * Request Lenses
    updateQuerySuggestionsConfig_attributeSuggestionsConfig,
    updateQuerySuggestionsConfig_includeQueriesWithoutUserInformation,
    updateQuerySuggestionsConfig_minimumNumberOfQueryingUsers,
    updateQuerySuggestionsConfig_minimumQueryCount,
    updateQuerySuggestionsConfig_mode,
    updateQuerySuggestionsConfig_queryLogLookBackWindowInDays,
    updateQuerySuggestionsConfig_indexId,

    -- * Destructuring the Response
    UpdateQuerySuggestionsConfigResponse (..),
    newUpdateQuerySuggestionsConfigResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateQuerySuggestionsConfig' smart constructor.
data UpdateQuerySuggestionsConfig = UpdateQuerySuggestionsConfig'
  { -- | Configuration information for the document fields\/attributes that you
    -- want to base query suggestions on.
    attributeSuggestionsConfig :: Prelude.Maybe AttributeSuggestionsUpdateConfig,
    -- | @TRUE@ to include queries without user information (i.e. all queries,
    -- irrespective of the user), otherwise @FALSE@ to only include queries
    -- with user information.
    --
    -- If you pass user information to Amazon Kendra along with the queries,
    -- you can set this flag to @FALSE@ and instruct Amazon Kendra to only
    -- consider queries with user information.
    --
    -- If you set to @FALSE@, Amazon Kendra only considers queries searched at
    -- least @MinimumQueryCount@ times across @MinimumNumberOfQueryingUsers@
    -- unique users for suggestions.
    --
    -- If you set to @TRUE@, Amazon Kendra ignores all user information and
    -- learns from all queries.
    includeQueriesWithoutUserInformation :: Prelude.Maybe Prelude.Bool,
    -- | The minimum number of unique users who must search a query in order for
    -- the query to be eligible to suggest to your users.
    --
    -- Increasing this number might decrease the number of suggestions.
    -- However, this ensures a query is searched by many users and is truly
    -- popular to suggest to users.
    --
    -- How you tune this setting depends on your specific needs.
    minimumNumberOfQueryingUsers :: Prelude.Maybe Prelude.Natural,
    -- | The the minimum number of times a query must be searched in order to be
    -- eligible to suggest to your users.
    --
    -- Decreasing this number increases the number of suggestions. However,
    -- this affects the quality of suggestions as it sets a low bar for a query
    -- to be considered popular to suggest to users.
    --
    -- How you tune this setting depends on your specific needs.
    minimumQueryCount :: Prelude.Maybe Prelude.Natural,
    -- | Set the mode to @ENABLED@ or @LEARN_ONLY@.
    --
    -- By default, Amazon Kendra enables query suggestions. @LEARN_ONLY@ mode
    -- allows you to turn off query suggestions. You can to update this at any
    -- time.
    --
    -- In @LEARN_ONLY@ mode, Amazon Kendra continues to learn from new queries
    -- to keep suggestions up to date for when you are ready to switch to
    -- ENABLED mode again.
    mode :: Prelude.Maybe Mode,
    -- | How recent your queries are in your query log time window.
    --
    -- The time window is the number of days from current day to past days.
    --
    -- By default, Amazon Kendra sets this to 180.
    queryLogLookBackWindowInDays :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the index with query suggestions you want to update.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateQuerySuggestionsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeSuggestionsConfig', 'updateQuerySuggestionsConfig_attributeSuggestionsConfig' - Configuration information for the document fields\/attributes that you
-- want to base query suggestions on.
--
-- 'includeQueriesWithoutUserInformation', 'updateQuerySuggestionsConfig_includeQueriesWithoutUserInformation' - @TRUE@ to include queries without user information (i.e. all queries,
-- irrespective of the user), otherwise @FALSE@ to only include queries
-- with user information.
--
-- If you pass user information to Amazon Kendra along with the queries,
-- you can set this flag to @FALSE@ and instruct Amazon Kendra to only
-- consider queries with user information.
--
-- If you set to @FALSE@, Amazon Kendra only considers queries searched at
-- least @MinimumQueryCount@ times across @MinimumNumberOfQueryingUsers@
-- unique users for suggestions.
--
-- If you set to @TRUE@, Amazon Kendra ignores all user information and
-- learns from all queries.
--
-- 'minimumNumberOfQueryingUsers', 'updateQuerySuggestionsConfig_minimumNumberOfQueryingUsers' - The minimum number of unique users who must search a query in order for
-- the query to be eligible to suggest to your users.
--
-- Increasing this number might decrease the number of suggestions.
-- However, this ensures a query is searched by many users and is truly
-- popular to suggest to users.
--
-- How you tune this setting depends on your specific needs.
--
-- 'minimumQueryCount', 'updateQuerySuggestionsConfig_minimumQueryCount' - The the minimum number of times a query must be searched in order to be
-- eligible to suggest to your users.
--
-- Decreasing this number increases the number of suggestions. However,
-- this affects the quality of suggestions as it sets a low bar for a query
-- to be considered popular to suggest to users.
--
-- How you tune this setting depends on your specific needs.
--
-- 'mode', 'updateQuerySuggestionsConfig_mode' - Set the mode to @ENABLED@ or @LEARN_ONLY@.
--
-- By default, Amazon Kendra enables query suggestions. @LEARN_ONLY@ mode
-- allows you to turn off query suggestions. You can to update this at any
-- time.
--
-- In @LEARN_ONLY@ mode, Amazon Kendra continues to learn from new queries
-- to keep suggestions up to date for when you are ready to switch to
-- ENABLED mode again.
--
-- 'queryLogLookBackWindowInDays', 'updateQuerySuggestionsConfig_queryLogLookBackWindowInDays' - How recent your queries are in your query log time window.
--
-- The time window is the number of days from current day to past days.
--
-- By default, Amazon Kendra sets this to 180.
--
-- 'indexId', 'updateQuerySuggestionsConfig_indexId' - The identifier of the index with query suggestions you want to update.
newUpdateQuerySuggestionsConfig ::
  -- | 'indexId'
  Prelude.Text ->
  UpdateQuerySuggestionsConfig
newUpdateQuerySuggestionsConfig pIndexId_ =
  UpdateQuerySuggestionsConfig'
    { attributeSuggestionsConfig =
        Prelude.Nothing,
      includeQueriesWithoutUserInformation =
        Prelude.Nothing,
      minimumNumberOfQueryingUsers =
        Prelude.Nothing,
      minimumQueryCount = Prelude.Nothing,
      mode = Prelude.Nothing,
      queryLogLookBackWindowInDays =
        Prelude.Nothing,
      indexId = pIndexId_
    }

-- | Configuration information for the document fields\/attributes that you
-- want to base query suggestions on.
updateQuerySuggestionsConfig_attributeSuggestionsConfig :: Lens.Lens' UpdateQuerySuggestionsConfig (Prelude.Maybe AttributeSuggestionsUpdateConfig)
updateQuerySuggestionsConfig_attributeSuggestionsConfig = Lens.lens (\UpdateQuerySuggestionsConfig' {attributeSuggestionsConfig} -> attributeSuggestionsConfig) (\s@UpdateQuerySuggestionsConfig' {} a -> s {attributeSuggestionsConfig = a} :: UpdateQuerySuggestionsConfig)

-- | @TRUE@ to include queries without user information (i.e. all queries,
-- irrespective of the user), otherwise @FALSE@ to only include queries
-- with user information.
--
-- If you pass user information to Amazon Kendra along with the queries,
-- you can set this flag to @FALSE@ and instruct Amazon Kendra to only
-- consider queries with user information.
--
-- If you set to @FALSE@, Amazon Kendra only considers queries searched at
-- least @MinimumQueryCount@ times across @MinimumNumberOfQueryingUsers@
-- unique users for suggestions.
--
-- If you set to @TRUE@, Amazon Kendra ignores all user information and
-- learns from all queries.
updateQuerySuggestionsConfig_includeQueriesWithoutUserInformation :: Lens.Lens' UpdateQuerySuggestionsConfig (Prelude.Maybe Prelude.Bool)
updateQuerySuggestionsConfig_includeQueriesWithoutUserInformation = Lens.lens (\UpdateQuerySuggestionsConfig' {includeQueriesWithoutUserInformation} -> includeQueriesWithoutUserInformation) (\s@UpdateQuerySuggestionsConfig' {} a -> s {includeQueriesWithoutUserInformation = a} :: UpdateQuerySuggestionsConfig)

-- | The minimum number of unique users who must search a query in order for
-- the query to be eligible to suggest to your users.
--
-- Increasing this number might decrease the number of suggestions.
-- However, this ensures a query is searched by many users and is truly
-- popular to suggest to users.
--
-- How you tune this setting depends on your specific needs.
updateQuerySuggestionsConfig_minimumNumberOfQueryingUsers :: Lens.Lens' UpdateQuerySuggestionsConfig (Prelude.Maybe Prelude.Natural)
updateQuerySuggestionsConfig_minimumNumberOfQueryingUsers = Lens.lens (\UpdateQuerySuggestionsConfig' {minimumNumberOfQueryingUsers} -> minimumNumberOfQueryingUsers) (\s@UpdateQuerySuggestionsConfig' {} a -> s {minimumNumberOfQueryingUsers = a} :: UpdateQuerySuggestionsConfig)

-- | The the minimum number of times a query must be searched in order to be
-- eligible to suggest to your users.
--
-- Decreasing this number increases the number of suggestions. However,
-- this affects the quality of suggestions as it sets a low bar for a query
-- to be considered popular to suggest to users.
--
-- How you tune this setting depends on your specific needs.
updateQuerySuggestionsConfig_minimumQueryCount :: Lens.Lens' UpdateQuerySuggestionsConfig (Prelude.Maybe Prelude.Natural)
updateQuerySuggestionsConfig_minimumQueryCount = Lens.lens (\UpdateQuerySuggestionsConfig' {minimumQueryCount} -> minimumQueryCount) (\s@UpdateQuerySuggestionsConfig' {} a -> s {minimumQueryCount = a} :: UpdateQuerySuggestionsConfig)

-- | Set the mode to @ENABLED@ or @LEARN_ONLY@.
--
-- By default, Amazon Kendra enables query suggestions. @LEARN_ONLY@ mode
-- allows you to turn off query suggestions. You can to update this at any
-- time.
--
-- In @LEARN_ONLY@ mode, Amazon Kendra continues to learn from new queries
-- to keep suggestions up to date for when you are ready to switch to
-- ENABLED mode again.
updateQuerySuggestionsConfig_mode :: Lens.Lens' UpdateQuerySuggestionsConfig (Prelude.Maybe Mode)
updateQuerySuggestionsConfig_mode = Lens.lens (\UpdateQuerySuggestionsConfig' {mode} -> mode) (\s@UpdateQuerySuggestionsConfig' {} a -> s {mode = a} :: UpdateQuerySuggestionsConfig)

-- | How recent your queries are in your query log time window.
--
-- The time window is the number of days from current day to past days.
--
-- By default, Amazon Kendra sets this to 180.
updateQuerySuggestionsConfig_queryLogLookBackWindowInDays :: Lens.Lens' UpdateQuerySuggestionsConfig (Prelude.Maybe Prelude.Int)
updateQuerySuggestionsConfig_queryLogLookBackWindowInDays = Lens.lens (\UpdateQuerySuggestionsConfig' {queryLogLookBackWindowInDays} -> queryLogLookBackWindowInDays) (\s@UpdateQuerySuggestionsConfig' {} a -> s {queryLogLookBackWindowInDays = a} :: UpdateQuerySuggestionsConfig)

-- | The identifier of the index with query suggestions you want to update.
updateQuerySuggestionsConfig_indexId :: Lens.Lens' UpdateQuerySuggestionsConfig Prelude.Text
updateQuerySuggestionsConfig_indexId = Lens.lens (\UpdateQuerySuggestionsConfig' {indexId} -> indexId) (\s@UpdateQuerySuggestionsConfig' {} a -> s {indexId = a} :: UpdateQuerySuggestionsConfig)

instance Core.AWSRequest UpdateQuerySuggestionsConfig where
  type
    AWSResponse UpdateQuerySuggestionsConfig =
      UpdateQuerySuggestionsConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateQuerySuggestionsConfigResponse'

instance
  Prelude.Hashable
    UpdateQuerySuggestionsConfig
  where
  hashWithSalt _salt UpdateQuerySuggestionsConfig' {..} =
    _salt
      `Prelude.hashWithSalt` attributeSuggestionsConfig
      `Prelude.hashWithSalt` includeQueriesWithoutUserInformation
      `Prelude.hashWithSalt` minimumNumberOfQueryingUsers
      `Prelude.hashWithSalt` minimumQueryCount
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` queryLogLookBackWindowInDays
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData UpdateQuerySuggestionsConfig where
  rnf UpdateQuerySuggestionsConfig' {..} =
    Prelude.rnf attributeSuggestionsConfig
      `Prelude.seq` Prelude.rnf includeQueriesWithoutUserInformation
      `Prelude.seq` Prelude.rnf minimumNumberOfQueryingUsers
      `Prelude.seq` Prelude.rnf minimumQueryCount
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf queryLogLookBackWindowInDays
      `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders UpdateQuerySuggestionsConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.UpdateQuerySuggestionsConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateQuerySuggestionsConfig where
  toJSON UpdateQuerySuggestionsConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeSuggestionsConfig" Data..=)
              Prelude.<$> attributeSuggestionsConfig,
            ("IncludeQueriesWithoutUserInformation" Data..=)
              Prelude.<$> includeQueriesWithoutUserInformation,
            ("MinimumNumberOfQueryingUsers" Data..=)
              Prelude.<$> minimumNumberOfQueryingUsers,
            ("MinimumQueryCount" Data..=)
              Prelude.<$> minimumQueryCount,
            ("Mode" Data..=) Prelude.<$> mode,
            ("QueryLogLookBackWindowInDays" Data..=)
              Prelude.<$> queryLogLookBackWindowInDays,
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath UpdateQuerySuggestionsConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateQuerySuggestionsConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateQuerySuggestionsConfigResponse' smart constructor.
data UpdateQuerySuggestionsConfigResponse = UpdateQuerySuggestionsConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateQuerySuggestionsConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQuerySuggestionsConfigResponse ::
  UpdateQuerySuggestionsConfigResponse
newUpdateQuerySuggestionsConfigResponse =
  UpdateQuerySuggestionsConfigResponse'

instance
  Prelude.NFData
    UpdateQuerySuggestionsConfigResponse
  where
  rnf _ = ()
