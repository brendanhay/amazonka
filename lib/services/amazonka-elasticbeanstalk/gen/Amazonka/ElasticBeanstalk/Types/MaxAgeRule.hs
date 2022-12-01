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
-- Module      : Amazonka.ElasticBeanstalk.Types.MaxAgeRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.MaxAgeRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A lifecycle rule that deletes application versions after the specified
-- number of days.
--
-- /See:/ 'newMaxAgeRule' smart constructor.
data MaxAgeRule = MaxAgeRule'
  { -- | Specify the number of days to retain an application versions.
    maxAgeInDays :: Prelude.Maybe Prelude.Int,
    -- | Set to @true@ to delete a version\'s source bundle from Amazon S3 when
    -- Elastic Beanstalk deletes the application version.
    deleteSourceFromS3 :: Prelude.Maybe Prelude.Bool,
    -- | Specify @true@ to apply the rule, or @false@ to disable it.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaxAgeRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxAgeInDays', 'maxAgeRule_maxAgeInDays' - Specify the number of days to retain an application versions.
--
-- 'deleteSourceFromS3', 'maxAgeRule_deleteSourceFromS3' - Set to @true@ to delete a version\'s source bundle from Amazon S3 when
-- Elastic Beanstalk deletes the application version.
--
-- 'enabled', 'maxAgeRule_enabled' - Specify @true@ to apply the rule, or @false@ to disable it.
newMaxAgeRule ::
  -- | 'enabled'
  Prelude.Bool ->
  MaxAgeRule
newMaxAgeRule pEnabled_ =
  MaxAgeRule'
    { maxAgeInDays = Prelude.Nothing,
      deleteSourceFromS3 = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Specify the number of days to retain an application versions.
maxAgeRule_maxAgeInDays :: Lens.Lens' MaxAgeRule (Prelude.Maybe Prelude.Int)
maxAgeRule_maxAgeInDays = Lens.lens (\MaxAgeRule' {maxAgeInDays} -> maxAgeInDays) (\s@MaxAgeRule' {} a -> s {maxAgeInDays = a} :: MaxAgeRule)

-- | Set to @true@ to delete a version\'s source bundle from Amazon S3 when
-- Elastic Beanstalk deletes the application version.
maxAgeRule_deleteSourceFromS3 :: Lens.Lens' MaxAgeRule (Prelude.Maybe Prelude.Bool)
maxAgeRule_deleteSourceFromS3 = Lens.lens (\MaxAgeRule' {deleteSourceFromS3} -> deleteSourceFromS3) (\s@MaxAgeRule' {} a -> s {deleteSourceFromS3 = a} :: MaxAgeRule)

-- | Specify @true@ to apply the rule, or @false@ to disable it.
maxAgeRule_enabled :: Lens.Lens' MaxAgeRule Prelude.Bool
maxAgeRule_enabled = Lens.lens (\MaxAgeRule' {enabled} -> enabled) (\s@MaxAgeRule' {} a -> s {enabled = a} :: MaxAgeRule)

instance Core.FromXML MaxAgeRule where
  parseXML x =
    MaxAgeRule'
      Prelude.<$> (x Core..@? "MaxAgeInDays")
      Prelude.<*> (x Core..@? "DeleteSourceFromS3")
      Prelude.<*> (x Core..@ "Enabled")

instance Prelude.Hashable MaxAgeRule where
  hashWithSalt _salt MaxAgeRule' {..} =
    _salt `Prelude.hashWithSalt` maxAgeInDays
      `Prelude.hashWithSalt` deleteSourceFromS3
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData MaxAgeRule where
  rnf MaxAgeRule' {..} =
    Prelude.rnf maxAgeInDays
      `Prelude.seq` Prelude.rnf deleteSourceFromS3
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToQuery MaxAgeRule where
  toQuery MaxAgeRule' {..} =
    Prelude.mconcat
      [ "MaxAgeInDays" Core.=: maxAgeInDays,
        "DeleteSourceFromS3" Core.=: deleteSourceFromS3,
        "Enabled" Core.=: enabled
      ]
