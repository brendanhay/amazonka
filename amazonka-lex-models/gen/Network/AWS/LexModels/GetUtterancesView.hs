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
-- Module      : Network.AWS.LexModels.GetUtterancesView
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @GetUtterancesView@ operation to get information about the
-- utterances that your users have made to your bot. You can use this list
-- to tune the utterances that your bot responds to.
--
-- For example, say that you have created a bot to order flowers. After
-- your users have used your bot for a while, use the @GetUtterancesView@
-- operation to see the requests that they have made and whether they have
-- been successful. You might find that the utterance \"I want flowers\" is
-- not being recognized. You could add this utterance to the @OrderFlowers@
-- intent so that your bot recognizes that utterance.
--
-- After you publish a new version of a bot, you can get information about
-- the old version and the new so that you can compare the performance
-- across the two versions.
--
-- Utterance statistics are generated once a day. Data is available for the
-- last 15 days. You can request information for up to 5 versions of your
-- bot in each request. Amazon Lex returns the most frequent utterances
-- received by the bot in the last 15 days. The response contains
-- information about a maximum of 100 utterances for each version.
--
-- If you set @childDirected@ field to true when you created your bot, or
-- if you opted out of participating in improving Amazon Lex, utterances
-- are not available.
--
-- This operation requires permissions for the @lex:GetUtterancesView@
-- action.
module Network.AWS.LexModels.GetUtterancesView
  ( -- * Creating a Request
    GetUtterancesView (..),
    newGetUtterancesView,

    -- * Request Lenses
    getUtterancesView_botName,
    getUtterancesView_botVersions,
    getUtterancesView_statusType,

    -- * Destructuring the Response
    GetUtterancesViewResponse (..),
    newGetUtterancesViewResponse,

    -- * Response Lenses
    getUtterancesViewResponse_botName,
    getUtterancesViewResponse_utterances,
    getUtterancesViewResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetUtterancesView' smart constructor.
data GetUtterancesView = GetUtterancesView'
  { -- | The name of the bot for which utterance information should be returned.
    botName :: Core.Text,
    -- | An array of bot versions for which utterance information should be
    -- returned. The limit is 5 versions per request.
    botVersions :: Core.NonEmpty Core.Text,
    -- | To return utterances that were recognized and handled, use @Detected@.
    -- To return utterances that were not recognized, use @Missed@.
    statusType :: StatusType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUtterancesView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botName', 'getUtterancesView_botName' - The name of the bot for which utterance information should be returned.
--
-- 'botVersions', 'getUtterancesView_botVersions' - An array of bot versions for which utterance information should be
-- returned. The limit is 5 versions per request.
--
-- 'statusType', 'getUtterancesView_statusType' - To return utterances that were recognized and handled, use @Detected@.
-- To return utterances that were not recognized, use @Missed@.
newGetUtterancesView ::
  -- | 'botName'
  Core.Text ->
  -- | 'botVersions'
  Core.NonEmpty Core.Text ->
  -- | 'statusType'
  StatusType ->
  GetUtterancesView
newGetUtterancesView
  pBotName_
  pBotVersions_
  pStatusType_ =
    GetUtterancesView'
      { botName = pBotName_,
        botVersions = Lens._Coerce Lens.# pBotVersions_,
        statusType = pStatusType_
      }

-- | The name of the bot for which utterance information should be returned.
getUtterancesView_botName :: Lens.Lens' GetUtterancesView Core.Text
getUtterancesView_botName = Lens.lens (\GetUtterancesView' {botName} -> botName) (\s@GetUtterancesView' {} a -> s {botName = a} :: GetUtterancesView)

-- | An array of bot versions for which utterance information should be
-- returned. The limit is 5 versions per request.
getUtterancesView_botVersions :: Lens.Lens' GetUtterancesView (Core.NonEmpty Core.Text)
getUtterancesView_botVersions = Lens.lens (\GetUtterancesView' {botVersions} -> botVersions) (\s@GetUtterancesView' {} a -> s {botVersions = a} :: GetUtterancesView) Core.. Lens._Coerce

-- | To return utterances that were recognized and handled, use @Detected@.
-- To return utterances that were not recognized, use @Missed@.
getUtterancesView_statusType :: Lens.Lens' GetUtterancesView StatusType
getUtterancesView_statusType = Lens.lens (\GetUtterancesView' {statusType} -> statusType) (\s@GetUtterancesView' {} a -> s {statusType = a} :: GetUtterancesView)

instance Core.AWSRequest GetUtterancesView where
  type
    AWSResponse GetUtterancesView =
      GetUtterancesViewResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUtterancesViewResponse'
            Core.<$> (x Core..?> "botName")
            Core.<*> (x Core..?> "utterances" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetUtterancesView

instance Core.NFData GetUtterancesView

instance Core.ToHeaders GetUtterancesView where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetUtterancesView where
  toPath GetUtterancesView' {..} =
    Core.mconcat
      ["/bots/", Core.toBS botName, "/utterances"]

instance Core.ToQuery GetUtterancesView where
  toQuery GetUtterancesView' {..} =
    Core.mconcat
      [ "bot_versions"
          Core.=: Core.toQueryList "member" botVersions,
        "status_type" Core.=: statusType,
        "view=aggregation"
      ]

-- | /See:/ 'newGetUtterancesViewResponse' smart constructor.
data GetUtterancesViewResponse = GetUtterancesViewResponse'
  { -- | The name of the bot for which utterance information was returned.
    botName :: Core.Maybe Core.Text,
    -- | An array of UtteranceList objects, each containing a list of
    -- UtteranceData objects describing the utterances that were processed by
    -- your bot. The response contains a maximum of 100 @UtteranceData@ objects
    -- for each version. Amazon Lex returns the most frequent utterances
    -- received by the bot in the last 15 days.
    utterances :: Core.Maybe [UtteranceList],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUtterancesViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botName', 'getUtterancesViewResponse_botName' - The name of the bot for which utterance information was returned.
--
-- 'utterances', 'getUtterancesViewResponse_utterances' - An array of UtteranceList objects, each containing a list of
-- UtteranceData objects describing the utterances that were processed by
-- your bot. The response contains a maximum of 100 @UtteranceData@ objects
-- for each version. Amazon Lex returns the most frequent utterances
-- received by the bot in the last 15 days.
--
-- 'httpStatus', 'getUtterancesViewResponse_httpStatus' - The response's http status code.
newGetUtterancesViewResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetUtterancesViewResponse
newGetUtterancesViewResponse pHttpStatus_ =
  GetUtterancesViewResponse'
    { botName = Core.Nothing,
      utterances = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the bot for which utterance information was returned.
getUtterancesViewResponse_botName :: Lens.Lens' GetUtterancesViewResponse (Core.Maybe Core.Text)
getUtterancesViewResponse_botName = Lens.lens (\GetUtterancesViewResponse' {botName} -> botName) (\s@GetUtterancesViewResponse' {} a -> s {botName = a} :: GetUtterancesViewResponse)

-- | An array of UtteranceList objects, each containing a list of
-- UtteranceData objects describing the utterances that were processed by
-- your bot. The response contains a maximum of 100 @UtteranceData@ objects
-- for each version. Amazon Lex returns the most frequent utterances
-- received by the bot in the last 15 days.
getUtterancesViewResponse_utterances :: Lens.Lens' GetUtterancesViewResponse (Core.Maybe [UtteranceList])
getUtterancesViewResponse_utterances = Lens.lens (\GetUtterancesViewResponse' {utterances} -> utterances) (\s@GetUtterancesViewResponse' {} a -> s {utterances = a} :: GetUtterancesViewResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getUtterancesViewResponse_httpStatus :: Lens.Lens' GetUtterancesViewResponse Core.Int
getUtterancesViewResponse_httpStatus = Lens.lens (\GetUtterancesViewResponse' {httpStatus} -> httpStatus) (\s@GetUtterancesViewResponse' {} a -> s {httpStatus = a} :: GetUtterancesViewResponse)

instance Core.NFData GetUtterancesViewResponse
