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
-- Module      : Network.AWS.ELBv2.Types.FixedResponseActionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.FixedResponseActionConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an action that returns a custom HTTP response.
--
-- /See:/ 'newFixedResponseActionConfig' smart constructor.
data FixedResponseActionConfig = FixedResponseActionConfig'
  { -- | The content type.
    --
    -- Valid Values: text\/plain | text\/css | text\/html |
    -- application\/javascript | application\/json
    contentType :: Core.Maybe Core.Text,
    -- | The message.
    messageBody :: Core.Maybe Core.Text,
    -- | The HTTP response code (2XX, 4XX, or 5XX).
    statusCode :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FixedResponseActionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'fixedResponseActionConfig_contentType' - The content type.
--
-- Valid Values: text\/plain | text\/css | text\/html |
-- application\/javascript | application\/json
--
-- 'messageBody', 'fixedResponseActionConfig_messageBody' - The message.
--
-- 'statusCode', 'fixedResponseActionConfig_statusCode' - The HTTP response code (2XX, 4XX, or 5XX).
newFixedResponseActionConfig ::
  -- | 'statusCode'
  Core.Text ->
  FixedResponseActionConfig
newFixedResponseActionConfig pStatusCode_ =
  FixedResponseActionConfig'
    { contentType =
        Core.Nothing,
      messageBody = Core.Nothing,
      statusCode = pStatusCode_
    }

-- | The content type.
--
-- Valid Values: text\/plain | text\/css | text\/html |
-- application\/javascript | application\/json
fixedResponseActionConfig_contentType :: Lens.Lens' FixedResponseActionConfig (Core.Maybe Core.Text)
fixedResponseActionConfig_contentType = Lens.lens (\FixedResponseActionConfig' {contentType} -> contentType) (\s@FixedResponseActionConfig' {} a -> s {contentType = a} :: FixedResponseActionConfig)

-- | The message.
fixedResponseActionConfig_messageBody :: Lens.Lens' FixedResponseActionConfig (Core.Maybe Core.Text)
fixedResponseActionConfig_messageBody = Lens.lens (\FixedResponseActionConfig' {messageBody} -> messageBody) (\s@FixedResponseActionConfig' {} a -> s {messageBody = a} :: FixedResponseActionConfig)

-- | The HTTP response code (2XX, 4XX, or 5XX).
fixedResponseActionConfig_statusCode :: Lens.Lens' FixedResponseActionConfig Core.Text
fixedResponseActionConfig_statusCode = Lens.lens (\FixedResponseActionConfig' {statusCode} -> statusCode) (\s@FixedResponseActionConfig' {} a -> s {statusCode = a} :: FixedResponseActionConfig)

instance Core.FromXML FixedResponseActionConfig where
  parseXML x =
    FixedResponseActionConfig'
      Core.<$> (x Core..@? "ContentType")
      Core.<*> (x Core..@? "MessageBody")
      Core.<*> (x Core..@ "StatusCode")

instance Core.Hashable FixedResponseActionConfig

instance Core.NFData FixedResponseActionConfig

instance Core.ToQuery FixedResponseActionConfig where
  toQuery FixedResponseActionConfig' {..} =
    Core.mconcat
      [ "ContentType" Core.=: contentType,
        "MessageBody" Core.=: messageBody,
        "StatusCode" Core.=: statusCode
      ]
