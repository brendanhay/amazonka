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
-- Module      : Network.AWS.ELBv2.Types.FixedResponseActionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.FixedResponseActionConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an action that returns a custom HTTP response.
--
-- /See:/ 'newFixedResponseActionConfig' smart constructor.
data FixedResponseActionConfig = FixedResponseActionConfig'
  { -- | The content type.
    --
    -- Valid Values: text\/plain | text\/css | text\/html |
    -- application\/javascript | application\/json
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The message.
    messageBody :: Prelude.Maybe Prelude.Text,
    -- | The HTTP response code (2XX, 4XX, or 5XX).
    statusCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  FixedResponseActionConfig
newFixedResponseActionConfig pStatusCode_ =
  FixedResponseActionConfig'
    { contentType =
        Prelude.Nothing,
      messageBody = Prelude.Nothing,
      statusCode = pStatusCode_
    }

-- | The content type.
--
-- Valid Values: text\/plain | text\/css | text\/html |
-- application\/javascript | application\/json
fixedResponseActionConfig_contentType :: Lens.Lens' FixedResponseActionConfig (Prelude.Maybe Prelude.Text)
fixedResponseActionConfig_contentType = Lens.lens (\FixedResponseActionConfig' {contentType} -> contentType) (\s@FixedResponseActionConfig' {} a -> s {contentType = a} :: FixedResponseActionConfig)

-- | The message.
fixedResponseActionConfig_messageBody :: Lens.Lens' FixedResponseActionConfig (Prelude.Maybe Prelude.Text)
fixedResponseActionConfig_messageBody = Lens.lens (\FixedResponseActionConfig' {messageBody} -> messageBody) (\s@FixedResponseActionConfig' {} a -> s {messageBody = a} :: FixedResponseActionConfig)

-- | The HTTP response code (2XX, 4XX, or 5XX).
fixedResponseActionConfig_statusCode :: Lens.Lens' FixedResponseActionConfig Prelude.Text
fixedResponseActionConfig_statusCode = Lens.lens (\FixedResponseActionConfig' {statusCode} -> statusCode) (\s@FixedResponseActionConfig' {} a -> s {statusCode = a} :: FixedResponseActionConfig)

instance Prelude.FromXML FixedResponseActionConfig where
  parseXML x =
    FixedResponseActionConfig'
      Prelude.<$> (x Prelude..@? "ContentType")
      Prelude.<*> (x Prelude..@? "MessageBody")
      Prelude.<*> (x Prelude..@ "StatusCode")

instance Prelude.Hashable FixedResponseActionConfig

instance Prelude.NFData FixedResponseActionConfig

instance Prelude.ToQuery FixedResponseActionConfig where
  toQuery FixedResponseActionConfig' {..} =
    Prelude.mconcat
      [ "ContentType" Prelude.=: contentType,
        "MessageBody" Prelude.=: messageBody,
        "StatusCode" Prelude.=: statusCode
      ]
