{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IVSChat.Types.MessageReviewHandler
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSChat.Types.MessageReviewHandler where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types.FallbackResult
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for optional message review.
--
-- /See:/ 'newMessageReviewHandler' smart constructor.
data MessageReviewHandler = MessageReviewHandler'
  { -- | Specifies the fallback behavior (whether the message is allowed or
    -- denied) if the handler does not return a valid response, encounters an
    -- error, or times out. (For the timeout period, see
    -- <https://docs.aws.amazon.com/ivs/latest/userguide/service-quotas.html Service Quotas>.)
    -- If allowed, the message is delivered with returned content to all users
    -- connected to the room. If denied, the message is not delivered to any
    -- user. Default: @ALLOW@.
    fallbackResult :: Prelude.Maybe FallbackResult,
    -- | Identifier of the message review handler. Currently this must be an ARN
    -- of a lambda function.
    uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageReviewHandler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fallbackResult', 'messageReviewHandler_fallbackResult' - Specifies the fallback behavior (whether the message is allowed or
-- denied) if the handler does not return a valid response, encounters an
-- error, or times out. (For the timeout period, see
-- <https://docs.aws.amazon.com/ivs/latest/userguide/service-quotas.html Service Quotas>.)
-- If allowed, the message is delivered with returned content to all users
-- connected to the room. If denied, the message is not delivered to any
-- user. Default: @ALLOW@.
--
-- 'uri', 'messageReviewHandler_uri' - Identifier of the message review handler. Currently this must be an ARN
-- of a lambda function.
newMessageReviewHandler ::
  MessageReviewHandler
newMessageReviewHandler =
  MessageReviewHandler'
    { fallbackResult =
        Prelude.Nothing,
      uri = Prelude.Nothing
    }

-- | Specifies the fallback behavior (whether the message is allowed or
-- denied) if the handler does not return a valid response, encounters an
-- error, or times out. (For the timeout period, see
-- <https://docs.aws.amazon.com/ivs/latest/userguide/service-quotas.html Service Quotas>.)
-- If allowed, the message is delivered with returned content to all users
-- connected to the room. If denied, the message is not delivered to any
-- user. Default: @ALLOW@.
messageReviewHandler_fallbackResult :: Lens.Lens' MessageReviewHandler (Prelude.Maybe FallbackResult)
messageReviewHandler_fallbackResult = Lens.lens (\MessageReviewHandler' {fallbackResult} -> fallbackResult) (\s@MessageReviewHandler' {} a -> s {fallbackResult = a} :: MessageReviewHandler)

-- | Identifier of the message review handler. Currently this must be an ARN
-- of a lambda function.
messageReviewHandler_uri :: Lens.Lens' MessageReviewHandler (Prelude.Maybe Prelude.Text)
messageReviewHandler_uri = Lens.lens (\MessageReviewHandler' {uri} -> uri) (\s@MessageReviewHandler' {} a -> s {uri = a} :: MessageReviewHandler)

instance Data.FromJSON MessageReviewHandler where
  parseJSON =
    Data.withObject
      "MessageReviewHandler"
      ( \x ->
          MessageReviewHandler'
            Prelude.<$> (x Data..:? "fallbackResult")
            Prelude.<*> (x Data..:? "uri")
      )

instance Prelude.Hashable MessageReviewHandler where
  hashWithSalt _salt MessageReviewHandler' {..} =
    _salt
      `Prelude.hashWithSalt` fallbackResult
      `Prelude.hashWithSalt` uri

instance Prelude.NFData MessageReviewHandler where
  rnf MessageReviewHandler' {..} =
    Prelude.rnf fallbackResult
      `Prelude.seq` Prelude.rnf uri

instance Data.ToJSON MessageReviewHandler where
  toJSON MessageReviewHandler' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fallbackResult" Data..=)
              Prelude.<$> fallbackResult,
            ("uri" Data..=) Prelude.<$> uri
          ]
      )
