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
-- Module      : Amazonka.IoT.Types.TopicRuleDestinationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TopicRuleDestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.HttpUrlDestinationConfiguration
import Amazonka.IoT.Types.VpcDestinationConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Configuration of the topic rule destination.
--
-- /See:/ 'newTopicRuleDestinationConfiguration' smart constructor.
data TopicRuleDestinationConfiguration = TopicRuleDestinationConfiguration'
  { -- | Configuration of the virtual private cloud (VPC) connection.
    vpcConfiguration :: Prelude.Maybe VpcDestinationConfiguration,
    -- | Configuration of the HTTP URL.
    httpUrlConfiguration :: Prelude.Maybe HttpUrlDestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicRuleDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfiguration', 'topicRuleDestinationConfiguration_vpcConfiguration' - Configuration of the virtual private cloud (VPC) connection.
--
-- 'httpUrlConfiguration', 'topicRuleDestinationConfiguration_httpUrlConfiguration' - Configuration of the HTTP URL.
newTopicRuleDestinationConfiguration ::
  TopicRuleDestinationConfiguration
newTopicRuleDestinationConfiguration =
  TopicRuleDestinationConfiguration'
    { vpcConfiguration =
        Prelude.Nothing,
      httpUrlConfiguration = Prelude.Nothing
    }

-- | Configuration of the virtual private cloud (VPC) connection.
topicRuleDestinationConfiguration_vpcConfiguration :: Lens.Lens' TopicRuleDestinationConfiguration (Prelude.Maybe VpcDestinationConfiguration)
topicRuleDestinationConfiguration_vpcConfiguration = Lens.lens (\TopicRuleDestinationConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@TopicRuleDestinationConfiguration' {} a -> s {vpcConfiguration = a} :: TopicRuleDestinationConfiguration)

-- | Configuration of the HTTP URL.
topicRuleDestinationConfiguration_httpUrlConfiguration :: Lens.Lens' TopicRuleDestinationConfiguration (Prelude.Maybe HttpUrlDestinationConfiguration)
topicRuleDestinationConfiguration_httpUrlConfiguration = Lens.lens (\TopicRuleDestinationConfiguration' {httpUrlConfiguration} -> httpUrlConfiguration) (\s@TopicRuleDestinationConfiguration' {} a -> s {httpUrlConfiguration = a} :: TopicRuleDestinationConfiguration)

instance
  Prelude.Hashable
    TopicRuleDestinationConfiguration
  where
  hashWithSalt
    _salt
    TopicRuleDestinationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` vpcConfiguration
        `Prelude.hashWithSalt` httpUrlConfiguration

instance
  Prelude.NFData
    TopicRuleDestinationConfiguration
  where
  rnf TopicRuleDestinationConfiguration' {..} =
    Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf httpUrlConfiguration

instance
  Core.ToJSON
    TopicRuleDestinationConfiguration
  where
  toJSON TopicRuleDestinationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("vpcConfiguration" Core..=)
              Prelude.<$> vpcConfiguration,
            ("httpUrlConfiguration" Core..=)
              Prelude.<$> httpUrlConfiguration
          ]
      )
