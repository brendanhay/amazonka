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
-- Module      : Network.AWS.ElasticBeanstalk.Types.Builder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Builder where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The builder used to build the custom platform.
--
-- /See:/ 'newBuilder' smart constructor.
data Builder = Builder'
  { -- | The ARN of the builder.
    arn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Builder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'builder_arn' - The ARN of the builder.
newBuilder ::
  Builder
newBuilder = Builder' {arn = Core.Nothing}

-- | The ARN of the builder.
builder_arn :: Lens.Lens' Builder (Core.Maybe Core.Text)
builder_arn = Lens.lens (\Builder' {arn} -> arn) (\s@Builder' {} a -> s {arn = a} :: Builder)

instance Core.FromXML Builder where
  parseXML x = Builder' Core.<$> (x Core..@? "ARN")

instance Core.Hashable Builder

instance Core.NFData Builder
