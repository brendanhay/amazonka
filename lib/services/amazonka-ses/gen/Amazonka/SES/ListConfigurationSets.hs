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
-- Module      : Amazonka.SES.ListConfigurationSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of the configuration sets associated with your Amazon
-- SES account in the current AWS Region. For information about using
-- configuration sets, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Monitoring Your Amazon SES Sending Activity>
-- in the /Amazon SES Developer Guide./
--
-- You can execute this operation no more than once per second. This
-- operation will return up to 1,000 configuration sets each time it is
-- run. If your Amazon SES account has more than 1,000 configuration sets,
-- this operation will also return a NextToken element. You can then
-- execute the @ListConfigurationSets@ operation again, passing the
-- @NextToken@ parameter and the value of the NextToken element to retrieve
-- additional results.
--
-- This operation returns paginated results.
module Amazonka.SES.ListConfigurationSets
  ( -- * Creating a Request
    ListConfigurationSets (..),
    newListConfigurationSets,

    -- * Request Lenses
    listConfigurationSets_maxItems,
    listConfigurationSets_nextToken,

    -- * Destructuring the Response
    ListConfigurationSetsResponse (..),
    newListConfigurationSetsResponse,

    -- * Response Lenses
    listConfigurationSetsResponse_configurationSets,
    listConfigurationSetsResponse_nextToken,
    listConfigurationSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to list the configuration sets associated with your
-- AWS account. Configuration sets enable you to publish email sending
-- events. For information about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newListConfigurationSets' smart constructor.
data ListConfigurationSets = ListConfigurationSets'
  { -- | The number of configuration sets to return.
    maxItems :: Prelude.Maybe Prelude.Int,
    -- | A token returned from a previous call to @ListConfigurationSets@ to
    -- indicate the position of the configuration set in the configuration set
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listConfigurationSets_maxItems' - The number of configuration sets to return.
--
-- 'nextToken', 'listConfigurationSets_nextToken' - A token returned from a previous call to @ListConfigurationSets@ to
-- indicate the position of the configuration set in the configuration set
-- list.
newListConfigurationSets ::
  ListConfigurationSets
newListConfigurationSets =
  ListConfigurationSets'
    { maxItems = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The number of configuration sets to return.
listConfigurationSets_maxItems :: Lens.Lens' ListConfigurationSets (Prelude.Maybe Prelude.Int)
listConfigurationSets_maxItems = Lens.lens (\ListConfigurationSets' {maxItems} -> maxItems) (\s@ListConfigurationSets' {} a -> s {maxItems = a} :: ListConfigurationSets)

-- | A token returned from a previous call to @ListConfigurationSets@ to
-- indicate the position of the configuration set in the configuration set
-- list.
listConfigurationSets_nextToken :: Lens.Lens' ListConfigurationSets (Prelude.Maybe Prelude.Text)
listConfigurationSets_nextToken = Lens.lens (\ListConfigurationSets' {nextToken} -> nextToken) (\s@ListConfigurationSets' {} a -> s {nextToken = a} :: ListConfigurationSets)

instance Core.AWSPager ListConfigurationSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConfigurationSetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listConfigurationSetsResponse_configurationSets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listConfigurationSets_nextToken
          Lens..~ rs
          Lens.^? listConfigurationSetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListConfigurationSets where
  type
    AWSResponse ListConfigurationSets =
      ListConfigurationSetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListConfigurationSetsResult"
      ( \s h x ->
          ListConfigurationSetsResponse'
            Prelude.<$> ( x Data..@? "ConfigurationSets"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConfigurationSets where
  hashWithSalt _salt ListConfigurationSets' {..} =
    _salt `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListConfigurationSets where
  rnf ListConfigurationSets' {..} =
    Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListConfigurationSets where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListConfigurationSets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListConfigurationSets where
  toQuery ListConfigurationSets' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListConfigurationSets" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "MaxItems" Data.=: maxItems,
        "NextToken" Data.=: nextToken
      ]

-- | A list of configuration sets associated with your AWS account.
-- Configuration sets enable you to publish email sending events. For
-- information about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newListConfigurationSetsResponse' smart constructor.
data ListConfigurationSetsResponse = ListConfigurationSetsResponse'
  { -- | A list of configuration sets.
    configurationSets :: Prelude.Maybe [ConfigurationSet],
    -- | A token indicating that there are additional configuration sets
    -- available to be listed. Pass this token to successive calls of
    -- @ListConfigurationSets@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigurationSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSets', 'listConfigurationSetsResponse_configurationSets' - A list of configuration sets.
--
-- 'nextToken', 'listConfigurationSetsResponse_nextToken' - A token indicating that there are additional configuration sets
-- available to be listed. Pass this token to successive calls of
-- @ListConfigurationSets@.
--
-- 'httpStatus', 'listConfigurationSetsResponse_httpStatus' - The response's http status code.
newListConfigurationSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConfigurationSetsResponse
newListConfigurationSetsResponse pHttpStatus_ =
  ListConfigurationSetsResponse'
    { configurationSets =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of configuration sets.
listConfigurationSetsResponse_configurationSets :: Lens.Lens' ListConfigurationSetsResponse (Prelude.Maybe [ConfigurationSet])
listConfigurationSetsResponse_configurationSets = Lens.lens (\ListConfigurationSetsResponse' {configurationSets} -> configurationSets) (\s@ListConfigurationSetsResponse' {} a -> s {configurationSets = a} :: ListConfigurationSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token indicating that there are additional configuration sets
-- available to be listed. Pass this token to successive calls of
-- @ListConfigurationSets@.
listConfigurationSetsResponse_nextToken :: Lens.Lens' ListConfigurationSetsResponse (Prelude.Maybe Prelude.Text)
listConfigurationSetsResponse_nextToken = Lens.lens (\ListConfigurationSetsResponse' {nextToken} -> nextToken) (\s@ListConfigurationSetsResponse' {} a -> s {nextToken = a} :: ListConfigurationSetsResponse)

-- | The response's http status code.
listConfigurationSetsResponse_httpStatus :: Lens.Lens' ListConfigurationSetsResponse Prelude.Int
listConfigurationSetsResponse_httpStatus = Lens.lens (\ListConfigurationSetsResponse' {httpStatus} -> httpStatus) (\s@ListConfigurationSetsResponse' {} a -> s {httpStatus = a} :: ListConfigurationSetsResponse)

instance Prelude.NFData ListConfigurationSetsResponse where
  rnf ListConfigurationSetsResponse' {..} =
    Prelude.rnf configurationSets
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
