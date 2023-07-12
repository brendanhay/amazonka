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
-- Module      : Amazonka.CodeStarNotifications.Types.Target
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarNotifications.Types.Target where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the Chatbot topics or Chatbot clients associated with
-- a notification rule.
--
-- /See:/ 'newTarget' smart constructor.
data Target = Target'
  { -- | The Amazon Resource Name (ARN) of the Chatbot topic or Chatbot client.
    targetAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The target type. Can be an Chatbot topic or Chatbot client.
    --
    -- -   Chatbot topics are specified as @SNS@.
    --
    -- -   Chatbot clients are specified as @AWSChatbotSlack@.
    targetType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Target' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetAddress', 'target_targetAddress' - The Amazon Resource Name (ARN) of the Chatbot topic or Chatbot client.
--
-- 'targetType', 'target_targetType' - The target type. Can be an Chatbot topic or Chatbot client.
--
-- -   Chatbot topics are specified as @SNS@.
--
-- -   Chatbot clients are specified as @AWSChatbotSlack@.
newTarget ::
  Target
newTarget =
  Target'
    { targetAddress = Prelude.Nothing,
      targetType = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Chatbot topic or Chatbot client.
target_targetAddress :: Lens.Lens' Target (Prelude.Maybe Prelude.Text)
target_targetAddress = Lens.lens (\Target' {targetAddress} -> targetAddress) (\s@Target' {} a -> s {targetAddress = a} :: Target) Prelude.. Lens.mapping Data._Sensitive

-- | The target type. Can be an Chatbot topic or Chatbot client.
--
-- -   Chatbot topics are specified as @SNS@.
--
-- -   Chatbot clients are specified as @AWSChatbotSlack@.
target_targetType :: Lens.Lens' Target (Prelude.Maybe Prelude.Text)
target_targetType = Lens.lens (\Target' {targetType} -> targetType) (\s@Target' {} a -> s {targetType = a} :: Target)

instance Prelude.Hashable Target where
  hashWithSalt _salt Target' {..} =
    _salt
      `Prelude.hashWithSalt` targetAddress
      `Prelude.hashWithSalt` targetType

instance Prelude.NFData Target where
  rnf Target' {..} =
    Prelude.rnf targetAddress
      `Prelude.seq` Prelude.rnf targetType

instance Data.ToJSON Target where
  toJSON Target' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetAddress" Data..=) Prelude.<$> targetAddress,
            ("TargetType" Data..=) Prelude.<$> targetType
          ]
      )
