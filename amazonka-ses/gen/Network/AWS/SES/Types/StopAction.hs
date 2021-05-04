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
-- Module      : Network.AWS.SES.Types.StopAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.StopAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.StopScope

-- | When included in a receipt rule, this action terminates the evaluation
-- of the receipt rule set and, optionally, publishes a notification to
-- Amazon Simple Notification Service (Amazon SNS).
--
-- For information about setting a stop action in a receipt rule, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-stop.html Amazon SES Developer Guide>.
--
-- /See:/ 'newStopAction' smart constructor.
data StopAction = StopAction'
  { -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
    -- the stop action is taken. An example of an Amazon SNS topic ARN is
    -- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
    -- Amazon SNS topics, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The scope of the StopAction. The only acceptable value is @RuleSet@.
    scope :: StopScope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'stopAction_topicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
-- the stop action is taken. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
--
-- 'scope', 'stopAction_scope' - The scope of the StopAction. The only acceptable value is @RuleSet@.
newStopAction ::
  -- | 'scope'
  StopScope ->
  StopAction
newStopAction pScope_ =
  StopAction'
    { topicArn = Prelude.Nothing,
      scope = pScope_
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
-- the stop action is taken. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
stopAction_topicArn :: Lens.Lens' StopAction (Prelude.Maybe Prelude.Text)
stopAction_topicArn = Lens.lens (\StopAction' {topicArn} -> topicArn) (\s@StopAction' {} a -> s {topicArn = a} :: StopAction)

-- | The scope of the StopAction. The only acceptable value is @RuleSet@.
stopAction_scope :: Lens.Lens' StopAction StopScope
stopAction_scope = Lens.lens (\StopAction' {scope} -> scope) (\s@StopAction' {} a -> s {scope = a} :: StopAction)

instance Prelude.FromXML StopAction where
  parseXML x =
    StopAction'
      Prelude.<$> (x Prelude..@? "TopicArn")
      Prelude.<*> (x Prelude..@ "Scope")

instance Prelude.Hashable StopAction

instance Prelude.NFData StopAction

instance Prelude.ToQuery StopAction where
  toQuery StopAction' {..} =
    Prelude.mconcat
      [ "TopicArn" Prelude.=: topicArn,
        "Scope" Prelude.=: scope
      ]
