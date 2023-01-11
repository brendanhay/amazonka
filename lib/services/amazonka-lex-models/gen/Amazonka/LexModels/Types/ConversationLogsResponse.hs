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
-- Module      : Amazonka.LexModels.Types.ConversationLogsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.ConversationLogsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types.LogSettingsResponse
import qualified Amazonka.Prelude as Prelude

-- | Contains information about conversation log settings.
--
-- /See:/ 'newConversationLogsResponse' smart constructor.
data ConversationLogsResponse = ConversationLogsResponse'
  { -- | The Amazon Resource Name (ARN) of the IAM role used to write your logs
    -- to CloudWatch Logs or an S3 bucket.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The settings for your conversation logs. You can log text, audio, or
    -- both.
    logSettings :: Prelude.Maybe [LogSettingsResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConversationLogsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamRoleArn', 'conversationLogsResponse_iamRoleArn' - The Amazon Resource Name (ARN) of the IAM role used to write your logs
-- to CloudWatch Logs or an S3 bucket.
--
-- 'logSettings', 'conversationLogsResponse_logSettings' - The settings for your conversation logs. You can log text, audio, or
-- both.
newConversationLogsResponse ::
  ConversationLogsResponse
newConversationLogsResponse =
  ConversationLogsResponse'
    { iamRoleArn =
        Prelude.Nothing,
      logSettings = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM role used to write your logs
-- to CloudWatch Logs or an S3 bucket.
conversationLogsResponse_iamRoleArn :: Lens.Lens' ConversationLogsResponse (Prelude.Maybe Prelude.Text)
conversationLogsResponse_iamRoleArn = Lens.lens (\ConversationLogsResponse' {iamRoleArn} -> iamRoleArn) (\s@ConversationLogsResponse' {} a -> s {iamRoleArn = a} :: ConversationLogsResponse)

-- | The settings for your conversation logs. You can log text, audio, or
-- both.
conversationLogsResponse_logSettings :: Lens.Lens' ConversationLogsResponse (Prelude.Maybe [LogSettingsResponse])
conversationLogsResponse_logSettings = Lens.lens (\ConversationLogsResponse' {logSettings} -> logSettings) (\s@ConversationLogsResponse' {} a -> s {logSettings = a} :: ConversationLogsResponse) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ConversationLogsResponse where
  parseJSON =
    Data.withObject
      "ConversationLogsResponse"
      ( \x ->
          ConversationLogsResponse'
            Prelude.<$> (x Data..:? "iamRoleArn")
            Prelude.<*> (x Data..:? "logSettings" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ConversationLogsResponse where
  hashWithSalt _salt ConversationLogsResponse' {..} =
    _salt `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` logSettings

instance Prelude.NFData ConversationLogsResponse where
  rnf ConversationLogsResponse' {..} =
    Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf logSettings
