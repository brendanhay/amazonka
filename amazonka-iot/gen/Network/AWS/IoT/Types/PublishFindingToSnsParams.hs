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
-- Module      : Network.AWS.IoT.Types.PublishFindingToSnsParams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PublishFindingToSnsParams where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Parameters to define a mitigation action that publishes findings to
-- Amazon SNS. You can implement your own custom actions in response to the
-- Amazon SNS messages.
--
-- /See:/ 'newPublishFindingToSnsParams' smart constructor.
data PublishFindingToSnsParams = PublishFindingToSnsParams'
  { -- | The ARN of the topic to which you want to publish the findings.
    topicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PublishFindingToSnsParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'publishFindingToSnsParams_topicArn' - The ARN of the topic to which you want to publish the findings.
newPublishFindingToSnsParams ::
  -- | 'topicArn'
  Prelude.Text ->
  PublishFindingToSnsParams
newPublishFindingToSnsParams pTopicArn_ =
  PublishFindingToSnsParams' {topicArn = pTopicArn_}

-- | The ARN of the topic to which you want to publish the findings.
publishFindingToSnsParams_topicArn :: Lens.Lens' PublishFindingToSnsParams Prelude.Text
publishFindingToSnsParams_topicArn = Lens.lens (\PublishFindingToSnsParams' {topicArn} -> topicArn) (\s@PublishFindingToSnsParams' {} a -> s {topicArn = a} :: PublishFindingToSnsParams)

instance Prelude.FromJSON PublishFindingToSnsParams where
  parseJSON =
    Prelude.withObject
      "PublishFindingToSnsParams"
      ( \x ->
          PublishFindingToSnsParams'
            Prelude.<$> (x Prelude..: "topicArn")
      )

instance Prelude.Hashable PublishFindingToSnsParams

instance Prelude.NFData PublishFindingToSnsParams

instance Prelude.ToJSON PublishFindingToSnsParams where
  toJSON PublishFindingToSnsParams' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("topicArn" Prelude..= topicArn)]
      )
