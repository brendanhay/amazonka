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
-- Module      : Network.AWS.SNS.Types.Topic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.Topic where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A wrapper type for the topic\'s Amazon Resource Name (ARN). To retrieve
-- a topic\'s attributes, use @GetTopicAttributes@.
--
-- /See:/ 'newTopic' smart constructor.
data Topic = Topic'
  { -- | The topic\'s ARN.
    topicArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Topic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'topic_topicArn' - The topic\'s ARN.
newTopic ::
  Topic
newTopic = Topic' {topicArn = Core.Nothing}

-- | The topic\'s ARN.
topic_topicArn :: Lens.Lens' Topic (Core.Maybe Core.Text)
topic_topicArn = Lens.lens (\Topic' {topicArn} -> topicArn) (\s@Topic' {} a -> s {topicArn = a} :: Topic)

instance Core.FromXML Topic where
  parseXML x = Topic' Core.<$> (x Core..@? "TopicArn")

instance Core.Hashable Topic

instance Core.NFData Topic
