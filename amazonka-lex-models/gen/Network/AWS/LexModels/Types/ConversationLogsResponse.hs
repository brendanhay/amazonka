{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.LexModels.Types.ConversationLogsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ConversationLogsResponse where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.LogSettingsResponse
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
conversationLogsResponse_logSettings = Lens.lens (\ConversationLogsResponse' {logSettings} -> logSettings) (\s@ConversationLogsResponse' {} a -> s {logSettings = a} :: ConversationLogsResponse) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ConversationLogsResponse where
  parseJSON =
    Prelude.withObject
      "ConversationLogsResponse"
      ( \x ->
          ConversationLogsResponse'
            Prelude.<$> (x Prelude..:? "iamRoleArn")
            Prelude.<*> ( x Prelude..:? "logSettings"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ConversationLogsResponse

instance Prelude.NFData ConversationLogsResponse
