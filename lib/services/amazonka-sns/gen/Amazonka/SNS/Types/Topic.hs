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
-- Module      : Amazonka.SNS.Types.Topic
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.Topic where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A wrapper type for the topic\'s Amazon Resource Name (ARN). To retrieve
-- a topic\'s attributes, use @GetTopicAttributes@.
--
-- /See:/ 'newTopic' smart constructor.
data Topic = Topic'
  { -- | The topic\'s ARN.
    topicArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
newTopic = Topic' {topicArn = Prelude.Nothing}

-- | The topic\'s ARN.
topic_topicArn :: Lens.Lens' Topic (Prelude.Maybe Prelude.Text)
topic_topicArn = Lens.lens (\Topic' {topicArn} -> topicArn) (\s@Topic' {} a -> s {topicArn = a} :: Topic)

instance Data.FromXML Topic where
  parseXML x =
    Topic' Prelude.<$> (x Data..@? "TopicArn")

instance Prelude.Hashable Topic where
  hashWithSalt _salt Topic' {..} =
    _salt `Prelude.hashWithSalt` topicArn

instance Prelude.NFData Topic where
  rnf Topic' {..} = Prelude.rnf topicArn
