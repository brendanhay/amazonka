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
-- Module      : Amazonka.LexModels.GetUtterancesView
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- If you set @childDirected@ field to true when you created your bot, if
-- you are using slot obfuscation with one or more slots, or if you opted
-- out of participating in improving Amazon Lex, utterances are not
-- available.
--
-- This operation requires permissions for the @lex:GetUtterancesView@
-- action.
module Amazonka.LexModels.GetUtterancesView
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUtterancesView' smart constructor.
data GetUtterancesView = GetUtterancesView'
  { -- | The name of the bot for which utterance information should be returned.
    botName :: Prelude.Text,
    -- | An array of bot versions for which utterance information should be
    -- returned. The limit is 5 versions per request.
    botVersions :: Prelude.NonEmpty Prelude.Text,
    -- | To return utterances that were recognized and handled, use @Detected@.
    -- To return utterances that were not recognized, use @Missed@.
    statusType :: StatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'botVersions'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'statusType'
  StatusType ->
  GetUtterancesView
newGetUtterancesView
  pBotName_
  pBotVersions_
  pStatusType_ =
    GetUtterancesView'
      { botName = pBotName_,
        botVersions = Lens.coerced Lens.# pBotVersions_,
        statusType = pStatusType_
      }

-- | The name of the bot for which utterance information should be returned.
getUtterancesView_botName :: Lens.Lens' GetUtterancesView Prelude.Text
getUtterancesView_botName = Lens.lens (\GetUtterancesView' {botName} -> botName) (\s@GetUtterancesView' {} a -> s {botName = a} :: GetUtterancesView)

-- | An array of bot versions for which utterance information should be
-- returned. The limit is 5 versions per request.
getUtterancesView_botVersions :: Lens.Lens' GetUtterancesView (Prelude.NonEmpty Prelude.Text)
getUtterancesView_botVersions = Lens.lens (\GetUtterancesView' {botVersions} -> botVersions) (\s@GetUtterancesView' {} a -> s {botVersions = a} :: GetUtterancesView) Prelude.. Lens.coerced

-- | To return utterances that were recognized and handled, use @Detected@.
-- To return utterances that were not recognized, use @Missed@.
getUtterancesView_statusType :: Lens.Lens' GetUtterancesView StatusType
getUtterancesView_statusType = Lens.lens (\GetUtterancesView' {statusType} -> statusType) (\s@GetUtterancesView' {} a -> s {statusType = a} :: GetUtterancesView)

instance Core.AWSRequest GetUtterancesView where
  type
    AWSResponse GetUtterancesView =
      GetUtterancesViewResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUtterancesViewResponse'
            Prelude.<$> (x Data..?> "botName")
            Prelude.<*> (x Data..?> "utterances" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUtterancesView where
  hashWithSalt _salt GetUtterancesView' {..} =
    _salt `Prelude.hashWithSalt` botName
      `Prelude.hashWithSalt` botVersions
      `Prelude.hashWithSalt` statusType

instance Prelude.NFData GetUtterancesView where
  rnf GetUtterancesView' {..} =
    Prelude.rnf botName
      `Prelude.seq` Prelude.rnf botVersions
      `Prelude.seq` Prelude.rnf statusType

instance Data.ToHeaders GetUtterancesView where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetUtterancesView where
  toPath GetUtterancesView' {..} =
    Prelude.mconcat
      ["/bots/", Data.toBS botName, "/utterances"]

instance Data.ToQuery GetUtterancesView where
  toQuery GetUtterancesView' {..} =
    Prelude.mconcat
      [ "bot_versions"
          Data.=: Data.toQueryList "member" botVersions,
        "status_type" Data.=: statusType,
        "view=aggregation"
      ]

-- | /See:/ 'newGetUtterancesViewResponse' smart constructor.
data GetUtterancesViewResponse = GetUtterancesViewResponse'
  { -- | The name of the bot for which utterance information was returned.
    botName :: Prelude.Maybe Prelude.Text,
    -- | An array of UtteranceList objects, each containing a list of
    -- UtteranceData objects describing the utterances that were processed by
    -- your bot. The response contains a maximum of 100 @UtteranceData@ objects
    -- for each version. Amazon Lex returns the most frequent utterances
    -- received by the bot in the last 15 days.
    utterances :: Prelude.Maybe [UtteranceList],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetUtterancesViewResponse
newGetUtterancesViewResponse pHttpStatus_ =
  GetUtterancesViewResponse'
    { botName =
        Prelude.Nothing,
      utterances = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the bot for which utterance information was returned.
getUtterancesViewResponse_botName :: Lens.Lens' GetUtterancesViewResponse (Prelude.Maybe Prelude.Text)
getUtterancesViewResponse_botName = Lens.lens (\GetUtterancesViewResponse' {botName} -> botName) (\s@GetUtterancesViewResponse' {} a -> s {botName = a} :: GetUtterancesViewResponse)

-- | An array of UtteranceList objects, each containing a list of
-- UtteranceData objects describing the utterances that were processed by
-- your bot. The response contains a maximum of 100 @UtteranceData@ objects
-- for each version. Amazon Lex returns the most frequent utterances
-- received by the bot in the last 15 days.
getUtterancesViewResponse_utterances :: Lens.Lens' GetUtterancesViewResponse (Prelude.Maybe [UtteranceList])
getUtterancesViewResponse_utterances = Lens.lens (\GetUtterancesViewResponse' {utterances} -> utterances) (\s@GetUtterancesViewResponse' {} a -> s {utterances = a} :: GetUtterancesViewResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getUtterancesViewResponse_httpStatus :: Lens.Lens' GetUtterancesViewResponse Prelude.Int
getUtterancesViewResponse_httpStatus = Lens.lens (\GetUtterancesViewResponse' {httpStatus} -> httpStatus) (\s@GetUtterancesViewResponse' {} a -> s {httpStatus = a} :: GetUtterancesViewResponse)

instance Prelude.NFData GetUtterancesViewResponse where
  rnf GetUtterancesViewResponse' {..} =
    Prelude.rnf botName
      `Prelude.seq` Prelude.rnf utterances
      `Prelude.seq` Prelude.rnf httpStatus
