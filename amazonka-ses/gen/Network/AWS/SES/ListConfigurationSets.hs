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
-- Module      : Network.AWS.SES.ListConfigurationSets
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SES.ListConfigurationSets
  ( -- * Creating a Request
    ListConfigurationSets (..),
    newListConfigurationSets,

    -- * Request Lenses
    listConfigurationSets_nextToken,
    listConfigurationSets_maxItems,

    -- * Destructuring the Response
    ListConfigurationSetsResponse (..),
    newListConfigurationSetsResponse,

    -- * Response Lenses
    listConfigurationSetsResponse_nextToken,
    listConfigurationSetsResponse_configurationSets,
    listConfigurationSetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to list the configuration sets associated with your
-- AWS account. Configuration sets enable you to publish email sending
-- events. For information about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newListConfigurationSets' smart constructor.
data ListConfigurationSets = ListConfigurationSets'
  { -- | A token returned from a previous call to @ListConfigurationSets@ to
    -- indicate the position of the configuration set in the configuration set
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The number of configuration sets to return.
    maxItems :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConfigurationSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigurationSets_nextToken' - A token returned from a previous call to @ListConfigurationSets@ to
-- indicate the position of the configuration set in the configuration set
-- list.
--
-- 'maxItems', 'listConfigurationSets_maxItems' - The number of configuration sets to return.
newListConfigurationSets ::
  ListConfigurationSets
newListConfigurationSets =
  ListConfigurationSets'
    { nextToken = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | A token returned from a previous call to @ListConfigurationSets@ to
-- indicate the position of the configuration set in the configuration set
-- list.
listConfigurationSets_nextToken :: Lens.Lens' ListConfigurationSets (Core.Maybe Core.Text)
listConfigurationSets_nextToken = Lens.lens (\ListConfigurationSets' {nextToken} -> nextToken) (\s@ListConfigurationSets' {} a -> s {nextToken = a} :: ListConfigurationSets)

-- | The number of configuration sets to return.
listConfigurationSets_maxItems :: Lens.Lens' ListConfigurationSets (Core.Maybe Core.Int)
listConfigurationSets_maxItems = Lens.lens (\ListConfigurationSets' {maxItems} -> maxItems) (\s@ListConfigurationSets' {} a -> s {maxItems = a} :: ListConfigurationSets)

instance Core.AWSPager ListConfigurationSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConfigurationSetsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listConfigurationSetsResponse_configurationSets
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listConfigurationSets_nextToken
          Lens..~ rs
          Lens.^? listConfigurationSetsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListConfigurationSets where
  type
    AWSResponse ListConfigurationSets =
      ListConfigurationSetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListConfigurationSetsResult"
      ( \s h x ->
          ListConfigurationSetsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "ConfigurationSets" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListConfigurationSets

instance Core.NFData ListConfigurationSets

instance Core.ToHeaders ListConfigurationSets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListConfigurationSets where
  toPath = Core.const "/"

instance Core.ToQuery ListConfigurationSets where
  toQuery ListConfigurationSets' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListConfigurationSets" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxItems" Core.=: maxItems
      ]

-- | A list of configuration sets associated with your AWS account.
-- Configuration sets enable you to publish email sending events. For
-- information about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newListConfigurationSetsResponse' smart constructor.
data ListConfigurationSetsResponse = ListConfigurationSetsResponse'
  { -- | A token indicating that there are additional configuration sets
    -- available to be listed. Pass this token to successive calls of
    -- @ListConfigurationSets@.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of configuration sets.
    configurationSets :: Core.Maybe [ConfigurationSet],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConfigurationSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigurationSetsResponse_nextToken' - A token indicating that there are additional configuration sets
-- available to be listed. Pass this token to successive calls of
-- @ListConfigurationSets@.
--
-- 'configurationSets', 'listConfigurationSetsResponse_configurationSets' - A list of configuration sets.
--
-- 'httpStatus', 'listConfigurationSetsResponse_httpStatus' - The response's http status code.
newListConfigurationSetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListConfigurationSetsResponse
newListConfigurationSetsResponse pHttpStatus_ =
  ListConfigurationSetsResponse'
    { nextToken =
        Core.Nothing,
      configurationSets = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token indicating that there are additional configuration sets
-- available to be listed. Pass this token to successive calls of
-- @ListConfigurationSets@.
listConfigurationSetsResponse_nextToken :: Lens.Lens' ListConfigurationSetsResponse (Core.Maybe Core.Text)
listConfigurationSetsResponse_nextToken = Lens.lens (\ListConfigurationSetsResponse' {nextToken} -> nextToken) (\s@ListConfigurationSetsResponse' {} a -> s {nextToken = a} :: ListConfigurationSetsResponse)

-- | A list of configuration sets.
listConfigurationSetsResponse_configurationSets :: Lens.Lens' ListConfigurationSetsResponse (Core.Maybe [ConfigurationSet])
listConfigurationSetsResponse_configurationSets = Lens.lens (\ListConfigurationSetsResponse' {configurationSets} -> configurationSets) (\s@ListConfigurationSetsResponse' {} a -> s {configurationSets = a} :: ListConfigurationSetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listConfigurationSetsResponse_httpStatus :: Lens.Lens' ListConfigurationSetsResponse Core.Int
listConfigurationSetsResponse_httpStatus = Lens.lens (\ListConfigurationSetsResponse' {httpStatus} -> httpStatus) (\s@ListConfigurationSetsResponse' {} a -> s {httpStatus = a} :: ListConfigurationSetsResponse)

instance Core.NFData ListConfigurationSetsResponse
