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
-- Module      : Amazonka.LexModels.DeleteUtterances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes stored utterances.
--
-- Amazon Lex stores the utterances that users send to your bot. Utterances
-- are stored for 15 days for use with the GetUtterancesView operation, and
-- then stored indefinitely for use in improving the ability of your bot to
-- respond to user input.
--
-- Use the @DeleteUtterances@ operation to manually delete stored
-- utterances for a specific user. When you use the @DeleteUtterances@
-- operation, utterances stored for improving your bot\'s ability to
-- respond to user input are deleted immediately. Utterances stored for use
-- with the @GetUtterancesView@ operation are deleted after 15 days.
--
-- This operation requires permissions for the @lex:DeleteUtterances@
-- action.
module Amazonka.LexModels.DeleteUtterances
  ( -- * Creating a Request
    DeleteUtterances (..),
    newDeleteUtterances,

    -- * Request Lenses
    deleteUtterances_botName,
    deleteUtterances_userId,

    -- * Destructuring the Response
    DeleteUtterancesResponse (..),
    newDeleteUtterancesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUtterances' smart constructor.
data DeleteUtterances = DeleteUtterances'
  { -- | The name of the bot that stored the utterances.
    botName :: Prelude.Text,
    -- | The unique identifier for the user that made the utterances. This is the
    -- user ID that was sent in the
    -- <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent>
    -- or
    -- <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
    -- operation request that contained the utterance.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUtterances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botName', 'deleteUtterances_botName' - The name of the bot that stored the utterances.
--
-- 'userId', 'deleteUtterances_userId' - The unique identifier for the user that made the utterances. This is the
-- user ID that was sent in the
-- <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent>
-- or
-- <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
-- operation request that contained the utterance.
newDeleteUtterances ::
  -- | 'botName'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  DeleteUtterances
newDeleteUtterances pBotName_ pUserId_ =
  DeleteUtterances'
    { botName = pBotName_,
      userId = pUserId_
    }

-- | The name of the bot that stored the utterances.
deleteUtterances_botName :: Lens.Lens' DeleteUtterances Prelude.Text
deleteUtterances_botName = Lens.lens (\DeleteUtterances' {botName} -> botName) (\s@DeleteUtterances' {} a -> s {botName = a} :: DeleteUtterances)

-- | The unique identifier for the user that made the utterances. This is the
-- user ID that was sent in the
-- <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent>
-- or
-- <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
-- operation request that contained the utterance.
deleteUtterances_userId :: Lens.Lens' DeleteUtterances Prelude.Text
deleteUtterances_userId = Lens.lens (\DeleteUtterances' {userId} -> userId) (\s@DeleteUtterances' {} a -> s {userId = a} :: DeleteUtterances)

instance Core.AWSRequest DeleteUtterances where
  type
    AWSResponse DeleteUtterances =
      DeleteUtterancesResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteUtterancesResponse'

instance Prelude.Hashable DeleteUtterances where
  hashWithSalt _salt DeleteUtterances' {..} =
    _salt
      `Prelude.hashWithSalt` botName
      `Prelude.hashWithSalt` userId

instance Prelude.NFData DeleteUtterances where
  rnf DeleteUtterances' {..} =
    Prelude.rnf botName
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders DeleteUtterances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteUtterances where
  toPath DeleteUtterances' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botName,
        "/utterances/",
        Data.toBS userId
      ]

instance Data.ToQuery DeleteUtterances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUtterancesResponse' smart constructor.
data DeleteUtterancesResponse = DeleteUtterancesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUtterancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUtterancesResponse ::
  DeleteUtterancesResponse
newDeleteUtterancesResponse =
  DeleteUtterancesResponse'

instance Prelude.NFData DeleteUtterancesResponse where
  rnf _ = ()
