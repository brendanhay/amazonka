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
-- Module      : Amazonka.IoT.Types.PublishFindingToSnsParams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.PublishFindingToSnsParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters to define a mitigation action that publishes findings to
-- Amazon SNS. You can implement your own custom actions in response to the
-- Amazon SNS messages.
--
-- /See:/ 'newPublishFindingToSnsParams' smart constructor.
data PublishFindingToSnsParams = PublishFindingToSnsParams'
  { -- | The ARN of the topic to which you want to publish the findings.
    topicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON PublishFindingToSnsParams where
  parseJSON =
    Data.withObject
      "PublishFindingToSnsParams"
      ( \x ->
          PublishFindingToSnsParams'
            Prelude.<$> (x Data..: "topicArn")
      )

instance Prelude.Hashable PublishFindingToSnsParams where
  hashWithSalt _salt PublishFindingToSnsParams' {..} =
    _salt `Prelude.hashWithSalt` topicArn

instance Prelude.NFData PublishFindingToSnsParams where
  rnf PublishFindingToSnsParams' {..} =
    Prelude.rnf topicArn

instance Data.ToJSON PublishFindingToSnsParams where
  toJSON PublishFindingToSnsParams' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("topicArn" Data..= topicArn)]
      )
