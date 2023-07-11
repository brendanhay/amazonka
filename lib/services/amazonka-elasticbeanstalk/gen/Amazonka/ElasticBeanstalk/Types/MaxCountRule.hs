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
-- Module      : Amazonka.ElasticBeanstalk.Types.MaxCountRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.MaxCountRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A lifecycle rule that deletes the oldest application version when the
-- maximum count is exceeded.
--
-- /See:/ 'newMaxCountRule' smart constructor.
data MaxCountRule = MaxCountRule'
  { -- | Set to @true@ to delete a version\'s source bundle from Amazon S3 when
    -- Elastic Beanstalk deletes the application version.
    deleteSourceFromS3 :: Prelude.Maybe Prelude.Bool,
    -- | Specify the maximum number of application versions to retain.
    maxCount :: Prelude.Maybe Prelude.Int,
    -- | Specify @true@ to apply the rule, or @false@ to disable it.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaxCountRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteSourceFromS3', 'maxCountRule_deleteSourceFromS3' - Set to @true@ to delete a version\'s source bundle from Amazon S3 when
-- Elastic Beanstalk deletes the application version.
--
-- 'maxCount', 'maxCountRule_maxCount' - Specify the maximum number of application versions to retain.
--
-- 'enabled', 'maxCountRule_enabled' - Specify @true@ to apply the rule, or @false@ to disable it.
newMaxCountRule ::
  -- | 'enabled'
  Prelude.Bool ->
  MaxCountRule
newMaxCountRule pEnabled_ =
  MaxCountRule'
    { deleteSourceFromS3 = Prelude.Nothing,
      maxCount = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Set to @true@ to delete a version\'s source bundle from Amazon S3 when
-- Elastic Beanstalk deletes the application version.
maxCountRule_deleteSourceFromS3 :: Lens.Lens' MaxCountRule (Prelude.Maybe Prelude.Bool)
maxCountRule_deleteSourceFromS3 = Lens.lens (\MaxCountRule' {deleteSourceFromS3} -> deleteSourceFromS3) (\s@MaxCountRule' {} a -> s {deleteSourceFromS3 = a} :: MaxCountRule)

-- | Specify the maximum number of application versions to retain.
maxCountRule_maxCount :: Lens.Lens' MaxCountRule (Prelude.Maybe Prelude.Int)
maxCountRule_maxCount = Lens.lens (\MaxCountRule' {maxCount} -> maxCount) (\s@MaxCountRule' {} a -> s {maxCount = a} :: MaxCountRule)

-- | Specify @true@ to apply the rule, or @false@ to disable it.
maxCountRule_enabled :: Lens.Lens' MaxCountRule Prelude.Bool
maxCountRule_enabled = Lens.lens (\MaxCountRule' {enabled} -> enabled) (\s@MaxCountRule' {} a -> s {enabled = a} :: MaxCountRule)

instance Data.FromXML MaxCountRule where
  parseXML x =
    MaxCountRule'
      Prelude.<$> (x Data..@? "DeleteSourceFromS3")
      Prelude.<*> (x Data..@? "MaxCount")
      Prelude.<*> (x Data..@ "Enabled")

instance Prelude.Hashable MaxCountRule where
  hashWithSalt _salt MaxCountRule' {..} =
    _salt
      `Prelude.hashWithSalt` deleteSourceFromS3
      `Prelude.hashWithSalt` maxCount
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData MaxCountRule where
  rnf MaxCountRule' {..} =
    Prelude.rnf deleteSourceFromS3
      `Prelude.seq` Prelude.rnf maxCount
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToQuery MaxCountRule where
  toQuery MaxCountRule' {..} =
    Prelude.mconcat
      [ "DeleteSourceFromS3" Data.=: deleteSourceFromS3,
        "MaxCount" Data.=: maxCount,
        "Enabled" Data.=: enabled
      ]
