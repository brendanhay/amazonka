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
-- Module      : Network.AWS.ElasticBeanstalk.Types.MaxCountRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.MaxCountRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A lifecycle rule that deletes the oldest application version when the
-- maximum count is exceeded.
--
-- /See:/ 'newMaxCountRule' smart constructor.
data MaxCountRule = MaxCountRule'
  { -- | Specify the maximum number of application versions to retain.
    maxCount :: Core.Maybe Core.Int,
    -- | Set to @true@ to delete a version\'s source bundle from Amazon S3 when
    -- Elastic Beanstalk deletes the application version.
    deleteSourceFromS3 :: Core.Maybe Core.Bool,
    -- | Specify @true@ to apply the rule, or @false@ to disable it.
    enabled :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MaxCountRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCount', 'maxCountRule_maxCount' - Specify the maximum number of application versions to retain.
--
-- 'deleteSourceFromS3', 'maxCountRule_deleteSourceFromS3' - Set to @true@ to delete a version\'s source bundle from Amazon S3 when
-- Elastic Beanstalk deletes the application version.
--
-- 'enabled', 'maxCountRule_enabled' - Specify @true@ to apply the rule, or @false@ to disable it.
newMaxCountRule ::
  -- | 'enabled'
  Core.Bool ->
  MaxCountRule
newMaxCountRule pEnabled_ =
  MaxCountRule'
    { maxCount = Core.Nothing,
      deleteSourceFromS3 = Core.Nothing,
      enabled = pEnabled_
    }

-- | Specify the maximum number of application versions to retain.
maxCountRule_maxCount :: Lens.Lens' MaxCountRule (Core.Maybe Core.Int)
maxCountRule_maxCount = Lens.lens (\MaxCountRule' {maxCount} -> maxCount) (\s@MaxCountRule' {} a -> s {maxCount = a} :: MaxCountRule)

-- | Set to @true@ to delete a version\'s source bundle from Amazon S3 when
-- Elastic Beanstalk deletes the application version.
maxCountRule_deleteSourceFromS3 :: Lens.Lens' MaxCountRule (Core.Maybe Core.Bool)
maxCountRule_deleteSourceFromS3 = Lens.lens (\MaxCountRule' {deleteSourceFromS3} -> deleteSourceFromS3) (\s@MaxCountRule' {} a -> s {deleteSourceFromS3 = a} :: MaxCountRule)

-- | Specify @true@ to apply the rule, or @false@ to disable it.
maxCountRule_enabled :: Lens.Lens' MaxCountRule Core.Bool
maxCountRule_enabled = Lens.lens (\MaxCountRule' {enabled} -> enabled) (\s@MaxCountRule' {} a -> s {enabled = a} :: MaxCountRule)

instance Core.FromXML MaxCountRule where
  parseXML x =
    MaxCountRule'
      Core.<$> (x Core..@? "MaxCount")
      Core.<*> (x Core..@? "DeleteSourceFromS3")
      Core.<*> (x Core..@ "Enabled")

instance Core.Hashable MaxCountRule

instance Core.NFData MaxCountRule

instance Core.ToQuery MaxCountRule where
  toQuery MaxCountRule' {..} =
    Core.mconcat
      [ "MaxCount" Core.=: maxCount,
        "DeleteSourceFromS3" Core.=: deleteSourceFromS3,
        "Enabled" Core.=: enabled
      ]
