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
-- Module      : Amazonka.TimeStreamQuery.Types.SnsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.SnsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details on SNS that are required to send the notification.
--
-- /See:/ 'newSnsConfiguration' smart constructor.
data SnsConfiguration = SnsConfiguration'
  { -- | SNS topic ARN that the scheduled query status notifications will be sent
    -- to.
    topicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'snsConfiguration_topicArn' - SNS topic ARN that the scheduled query status notifications will be sent
-- to.
newSnsConfiguration ::
  -- | 'topicArn'
  Prelude.Text ->
  SnsConfiguration
newSnsConfiguration pTopicArn_ =
  SnsConfiguration' {topicArn = pTopicArn_}

-- | SNS topic ARN that the scheduled query status notifications will be sent
-- to.
snsConfiguration_topicArn :: Lens.Lens' SnsConfiguration Prelude.Text
snsConfiguration_topicArn = Lens.lens (\SnsConfiguration' {topicArn} -> topicArn) (\s@SnsConfiguration' {} a -> s {topicArn = a} :: SnsConfiguration)

instance Data.FromJSON SnsConfiguration where
  parseJSON =
    Data.withObject
      "SnsConfiguration"
      ( \x ->
          SnsConfiguration' Prelude.<$> (x Data..: "TopicArn")
      )

instance Prelude.Hashable SnsConfiguration where
  hashWithSalt _salt SnsConfiguration' {..} =
    _salt `Prelude.hashWithSalt` topicArn

instance Prelude.NFData SnsConfiguration where
  rnf SnsConfiguration' {..} = Prelude.rnf topicArn

instance Data.ToJSON SnsConfiguration where
  toJSON SnsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TopicArn" Data..= topicArn)]
      )
