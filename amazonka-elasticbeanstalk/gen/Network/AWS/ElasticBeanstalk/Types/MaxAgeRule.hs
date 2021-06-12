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
-- Module      : Network.AWS.ElasticBeanstalk.Types.MaxAgeRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.MaxAgeRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A lifecycle rule that deletes application versions after the specified
-- number of days.
--
-- /See:/ 'newMaxAgeRule' smart constructor.
data MaxAgeRule = MaxAgeRule'
  { -- | Set to @true@ to delete a version\'s source bundle from Amazon S3 when
    -- Elastic Beanstalk deletes the application version.
    deleteSourceFromS3 :: Core.Maybe Core.Bool,
    -- | Specify the number of days to retain an application versions.
    maxAgeInDays :: Core.Maybe Core.Int,
    -- | Specify @true@ to apply the rule, or @false@ to disable it.
    enabled :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MaxAgeRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteSourceFromS3', 'maxAgeRule_deleteSourceFromS3' - Set to @true@ to delete a version\'s source bundle from Amazon S3 when
-- Elastic Beanstalk deletes the application version.
--
-- 'maxAgeInDays', 'maxAgeRule_maxAgeInDays' - Specify the number of days to retain an application versions.
--
-- 'enabled', 'maxAgeRule_enabled' - Specify @true@ to apply the rule, or @false@ to disable it.
newMaxAgeRule ::
  -- | 'enabled'
  Core.Bool ->
  MaxAgeRule
newMaxAgeRule pEnabled_ =
  MaxAgeRule'
    { deleteSourceFromS3 = Core.Nothing,
      maxAgeInDays = Core.Nothing,
      enabled = pEnabled_
    }

-- | Set to @true@ to delete a version\'s source bundle from Amazon S3 when
-- Elastic Beanstalk deletes the application version.
maxAgeRule_deleteSourceFromS3 :: Lens.Lens' MaxAgeRule (Core.Maybe Core.Bool)
maxAgeRule_deleteSourceFromS3 = Lens.lens (\MaxAgeRule' {deleteSourceFromS3} -> deleteSourceFromS3) (\s@MaxAgeRule' {} a -> s {deleteSourceFromS3 = a} :: MaxAgeRule)

-- | Specify the number of days to retain an application versions.
maxAgeRule_maxAgeInDays :: Lens.Lens' MaxAgeRule (Core.Maybe Core.Int)
maxAgeRule_maxAgeInDays = Lens.lens (\MaxAgeRule' {maxAgeInDays} -> maxAgeInDays) (\s@MaxAgeRule' {} a -> s {maxAgeInDays = a} :: MaxAgeRule)

-- | Specify @true@ to apply the rule, or @false@ to disable it.
maxAgeRule_enabled :: Lens.Lens' MaxAgeRule Core.Bool
maxAgeRule_enabled = Lens.lens (\MaxAgeRule' {enabled} -> enabled) (\s@MaxAgeRule' {} a -> s {enabled = a} :: MaxAgeRule)

instance Core.FromXML MaxAgeRule where
  parseXML x =
    MaxAgeRule'
      Core.<$> (x Core..@? "DeleteSourceFromS3")
      Core.<*> (x Core..@? "MaxAgeInDays")
      Core.<*> (x Core..@ "Enabled")

instance Core.Hashable MaxAgeRule

instance Core.NFData MaxAgeRule

instance Core.ToQuery MaxAgeRule where
  toQuery MaxAgeRule' {..} =
    Core.mconcat
      [ "DeleteSourceFromS3" Core.=: deleteSourceFromS3,
        "MaxAgeInDays" Core.=: maxAgeInDays,
        "Enabled" Core.=: enabled
      ]
