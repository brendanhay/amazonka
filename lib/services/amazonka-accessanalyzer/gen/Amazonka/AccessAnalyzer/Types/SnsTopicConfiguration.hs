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
-- Module      : Amazonka.AccessAnalyzer.Types.SnsTopicConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.SnsTopicConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The proposed access control configuration for an Amazon SNS topic. You
-- can propose a configuration for a new Amazon SNS topic or an existing
-- Amazon SNS topic that you own by specifying the policy. If the
-- configuration is for an existing Amazon SNS topic and you do not specify
-- the Amazon SNS policy, then the access preview uses the existing Amazon
-- SNS policy for the topic. If the access preview is for a new resource
-- and you do not specify the policy, then the access preview assumes an
-- Amazon SNS topic without a policy. To propose deletion of an existing
-- Amazon SNS topic policy, you can specify an empty string for the Amazon
-- SNS policy. For more information, see
-- <https://docs.aws.amazon.com/sns/latest/api/API_Topic.html Topic>.
--
-- /See:/ 'newSnsTopicConfiguration' smart constructor.
data SnsTopicConfiguration = SnsTopicConfiguration'
  { -- | The JSON policy text that defines who can access an Amazon SNS topic.
    -- For more information, see
    -- <https://docs.aws.amazon.com/sns/latest/dg/sns-access-policy-use-cases.html Example cases for Amazon SNS access control>
    -- in the /Amazon SNS Developer Guide/.
    topicPolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnsTopicConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicPolicy', 'snsTopicConfiguration_topicPolicy' - The JSON policy text that defines who can access an Amazon SNS topic.
-- For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-access-policy-use-cases.html Example cases for Amazon SNS access control>
-- in the /Amazon SNS Developer Guide/.
newSnsTopicConfiguration ::
  SnsTopicConfiguration
newSnsTopicConfiguration =
  SnsTopicConfiguration'
    { topicPolicy =
        Prelude.Nothing
    }

-- | The JSON policy text that defines who can access an Amazon SNS topic.
-- For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-access-policy-use-cases.html Example cases for Amazon SNS access control>
-- in the /Amazon SNS Developer Guide/.
snsTopicConfiguration_topicPolicy :: Lens.Lens' SnsTopicConfiguration (Prelude.Maybe Prelude.Text)
snsTopicConfiguration_topicPolicy = Lens.lens (\SnsTopicConfiguration' {topicPolicy} -> topicPolicy) (\s@SnsTopicConfiguration' {} a -> s {topicPolicy = a} :: SnsTopicConfiguration)

instance Data.FromJSON SnsTopicConfiguration where
  parseJSON =
    Data.withObject
      "SnsTopicConfiguration"
      ( \x ->
          SnsTopicConfiguration'
            Prelude.<$> (x Data..:? "topicPolicy")
      )

instance Prelude.Hashable SnsTopicConfiguration where
  hashWithSalt _salt SnsTopicConfiguration' {..} =
    _salt `Prelude.hashWithSalt` topicPolicy

instance Prelude.NFData SnsTopicConfiguration where
  rnf SnsTopicConfiguration' {..} =
    Prelude.rnf topicPolicy

instance Data.ToJSON SnsTopicConfiguration where
  toJSON SnsTopicConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("topicPolicy" Data..=) Prelude.<$> topicPolicy]
      )
