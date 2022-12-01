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
-- Module      : Amazonka.LexV2Models.DeleteUtterances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes stored utterances.
--
-- Amazon Lex stores the utterances that users send to your bot. Utterances
-- are stored for 15 days for use with the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_ListAggregatedUtterances.html ListAggregatedUtterances>
-- operation, and then stored indefinitely for use in improving the ability
-- of your bot to respond to user input..
--
-- Use the @DeleteUtterances@ operation to manually delete utterances for a
-- specific session. When you use the @DeleteUtterances@ operation,
-- utterances stored for improving your bot\'s ability to respond to user
-- input are deleted immediately. Utterances stored for use with the
-- @ListAggregatedUtterances@ operation are deleted after 15 days.
module Amazonka.LexV2Models.DeleteUtterances
  ( -- * Creating a Request
    DeleteUtterances (..),
    newDeleteUtterances,

    -- * Request Lenses
    deleteUtterances_localeId,
    deleteUtterances_sessionId,
    deleteUtterances_botId,

    -- * Destructuring the Response
    DeleteUtterancesResponse (..),
    newDeleteUtterancesResponse,

    -- * Response Lenses
    deleteUtterancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUtterances' smart constructor.
data DeleteUtterances = DeleteUtterances'
  { -- | The identifier of the language and locale where the utterances were
    -- collected. The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the session with the user. The ID is returned
    -- in the response from the
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/API_runtime_RecognizeText.html RecognizeText>
    -- and
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/API_runtime_RecognizeUtterance.html RecognizeUtterance>
    -- operations.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the bot that contains the utterances.
    botId :: Prelude.Text
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
-- 'localeId', 'deleteUtterances_localeId' - The identifier of the language and locale where the utterances were
-- collected. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
--
-- 'sessionId', 'deleteUtterances_sessionId' - The unique identifier of the session with the user. The ID is returned
-- in the response from the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_runtime_RecognizeText.html RecognizeText>
-- and
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_runtime_RecognizeUtterance.html RecognizeUtterance>
-- operations.
--
-- 'botId', 'deleteUtterances_botId' - The unique identifier of the bot that contains the utterances.
newDeleteUtterances ::
  -- | 'botId'
  Prelude.Text ->
  DeleteUtterances
newDeleteUtterances pBotId_ =
  DeleteUtterances'
    { localeId = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      botId = pBotId_
    }

-- | The identifier of the language and locale where the utterances were
-- collected. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
deleteUtterances_localeId :: Lens.Lens' DeleteUtterances (Prelude.Maybe Prelude.Text)
deleteUtterances_localeId = Lens.lens (\DeleteUtterances' {localeId} -> localeId) (\s@DeleteUtterances' {} a -> s {localeId = a} :: DeleteUtterances)

-- | The unique identifier of the session with the user. The ID is returned
-- in the response from the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_runtime_RecognizeText.html RecognizeText>
-- and
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_runtime_RecognizeUtterance.html RecognizeUtterance>
-- operations.
deleteUtterances_sessionId :: Lens.Lens' DeleteUtterances (Prelude.Maybe Prelude.Text)
deleteUtterances_sessionId = Lens.lens (\DeleteUtterances' {sessionId} -> sessionId) (\s@DeleteUtterances' {} a -> s {sessionId = a} :: DeleteUtterances)

-- | The unique identifier of the bot that contains the utterances.
deleteUtterances_botId :: Lens.Lens' DeleteUtterances Prelude.Text
deleteUtterances_botId = Lens.lens (\DeleteUtterances' {botId} -> botId) (\s@DeleteUtterances' {} a -> s {botId = a} :: DeleteUtterances)

instance Core.AWSRequest DeleteUtterances where
  type
    AWSResponse DeleteUtterances =
      DeleteUtterancesResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUtterancesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteUtterances where
  hashWithSalt _salt DeleteUtterances' {..} =
    _salt `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` botId

instance Prelude.NFData DeleteUtterances where
  rnf DeleteUtterances' {..} =
    Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf botId

instance Core.ToHeaders DeleteUtterances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteUtterances where
  toPath DeleteUtterances' {..} =
    Prelude.mconcat
      ["/bots/", Core.toBS botId, "/utterances/"]

instance Core.ToQuery DeleteUtterances where
  toQuery DeleteUtterances' {..} =
    Prelude.mconcat
      [ "localeId" Core.=: localeId,
        "sessionId" Core.=: sessionId
      ]

-- | /See:/ 'newDeleteUtterancesResponse' smart constructor.
data DeleteUtterancesResponse = DeleteUtterancesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUtterancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUtterancesResponse_httpStatus' - The response's http status code.
newDeleteUtterancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteUtterancesResponse
newDeleteUtterancesResponse pHttpStatus_ =
  DeleteUtterancesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteUtterancesResponse_httpStatus :: Lens.Lens' DeleteUtterancesResponse Prelude.Int
deleteUtterancesResponse_httpStatus = Lens.lens (\DeleteUtterancesResponse' {httpStatus} -> httpStatus) (\s@DeleteUtterancesResponse' {} a -> s {httpStatus = a} :: DeleteUtterancesResponse)

instance Prelude.NFData DeleteUtterancesResponse where
  rnf DeleteUtterancesResponse' {..} =
    Prelude.rnf httpStatus
