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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ResourceQuota
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ResourceQuota where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The AWS Elastic Beanstalk quota information for a single resource type
-- in an AWS account. It reflects the resource\'s limits for this account.
--
-- /See:/ 'newResourceQuota' smart constructor.
data ResourceQuota = ResourceQuota'
  { -- | The maximum number of instances of this Elastic Beanstalk resource type
    -- that an AWS account can use.
    maximum :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceQuota' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximum', 'resourceQuota_maximum' - The maximum number of instances of this Elastic Beanstalk resource type
-- that an AWS account can use.
newResourceQuota ::
  ResourceQuota
newResourceQuota =
  ResourceQuota' {maximum = Core.Nothing}

-- | The maximum number of instances of this Elastic Beanstalk resource type
-- that an AWS account can use.
resourceQuota_maximum :: Lens.Lens' ResourceQuota (Core.Maybe Core.Int)
resourceQuota_maximum = Lens.lens (\ResourceQuota' {maximum} -> maximum) (\s@ResourceQuota' {} a -> s {maximum = a} :: ResourceQuota)

instance Core.FromXML ResourceQuota where
  parseXML x =
    ResourceQuota' Core.<$> (x Core..@? "Maximum")

instance Core.Hashable ResourceQuota

instance Core.NFData ResourceQuota
