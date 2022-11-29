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
-- Module      : Amazonka.Connect.Types.ChatStreamingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ChatStreamingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The streaming configuration, such as the Amazon SNS streaming endpoint.
--
-- /See:/ 'newChatStreamingConfiguration' smart constructor.
data ChatStreamingConfiguration = ChatStreamingConfiguration'
  { -- | The Amazon Resource Name (ARN) of the standard Amazon SNS topic. The
    -- Amazon Resource Name (ARN) of the streaming endpoint that is used to
    -- publish real-time message streaming for chat conversations.
    streamingEndpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChatStreamingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingEndpointArn', 'chatStreamingConfiguration_streamingEndpointArn' - The Amazon Resource Name (ARN) of the standard Amazon SNS topic. The
-- Amazon Resource Name (ARN) of the streaming endpoint that is used to
-- publish real-time message streaming for chat conversations.
newChatStreamingConfiguration ::
  -- | 'streamingEndpointArn'
  Prelude.Text ->
  ChatStreamingConfiguration
newChatStreamingConfiguration pStreamingEndpointArn_ =
  ChatStreamingConfiguration'
    { streamingEndpointArn =
        pStreamingEndpointArn_
    }

-- | The Amazon Resource Name (ARN) of the standard Amazon SNS topic. The
-- Amazon Resource Name (ARN) of the streaming endpoint that is used to
-- publish real-time message streaming for chat conversations.
chatStreamingConfiguration_streamingEndpointArn :: Lens.Lens' ChatStreamingConfiguration Prelude.Text
chatStreamingConfiguration_streamingEndpointArn = Lens.lens (\ChatStreamingConfiguration' {streamingEndpointArn} -> streamingEndpointArn) (\s@ChatStreamingConfiguration' {} a -> s {streamingEndpointArn = a} :: ChatStreamingConfiguration)

instance Prelude.Hashable ChatStreamingConfiguration where
  hashWithSalt _salt ChatStreamingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` streamingEndpointArn

instance Prelude.NFData ChatStreamingConfiguration where
  rnf ChatStreamingConfiguration' {..} =
    Prelude.rnf streamingEndpointArn

instance Core.ToJSON ChatStreamingConfiguration where
  toJSON ChatStreamingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StreamingEndpointArn"
                  Core..= streamingEndpointArn
              )
          ]
      )
