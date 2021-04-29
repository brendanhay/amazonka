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
-- Module      : Network.AWS.MediaStore.Types.MetricPolicyRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.MetricPolicyRule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A setting that enables metrics at the object level. Each rule contains
-- an object group and an object group name. If the policy includes the
-- MetricPolicyRules parameter, you must include at least one rule. Each
-- metric policy can include up to five rules by default. You can also
-- <https://console.aws.amazon.com/servicequotas/home?region=us-east-1#!/services/mediastore/quotas request a quota increase>
-- to allow up to 300 rules per policy.
--
-- /See:/ 'newMetricPolicyRule' smart constructor.
data MetricPolicyRule = MetricPolicyRule'
  { -- | A path or file name that defines which objects to include in the group.
    -- Wildcards (*) are acceptable.
    objectGroup :: Prelude.Text,
    -- | A name that allows you to refer to the object group.
    objectGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MetricPolicyRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectGroup', 'metricPolicyRule_objectGroup' - A path or file name that defines which objects to include in the group.
-- Wildcards (*) are acceptable.
--
-- 'objectGroupName', 'metricPolicyRule_objectGroupName' - A name that allows you to refer to the object group.
newMetricPolicyRule ::
  -- | 'objectGroup'
  Prelude.Text ->
  -- | 'objectGroupName'
  Prelude.Text ->
  MetricPolicyRule
newMetricPolicyRule pObjectGroup_ pObjectGroupName_ =
  MetricPolicyRule'
    { objectGroup = pObjectGroup_,
      objectGroupName = pObjectGroupName_
    }

-- | A path or file name that defines which objects to include in the group.
-- Wildcards (*) are acceptable.
metricPolicyRule_objectGroup :: Lens.Lens' MetricPolicyRule Prelude.Text
metricPolicyRule_objectGroup = Lens.lens (\MetricPolicyRule' {objectGroup} -> objectGroup) (\s@MetricPolicyRule' {} a -> s {objectGroup = a} :: MetricPolicyRule)

-- | A name that allows you to refer to the object group.
metricPolicyRule_objectGroupName :: Lens.Lens' MetricPolicyRule Prelude.Text
metricPolicyRule_objectGroupName = Lens.lens (\MetricPolicyRule' {objectGroupName} -> objectGroupName) (\s@MetricPolicyRule' {} a -> s {objectGroupName = a} :: MetricPolicyRule)

instance Prelude.FromJSON MetricPolicyRule where
  parseJSON =
    Prelude.withObject
      "MetricPolicyRule"
      ( \x ->
          MetricPolicyRule'
            Prelude.<$> (x Prelude..: "ObjectGroup")
            Prelude.<*> (x Prelude..: "ObjectGroupName")
      )

instance Prelude.Hashable MetricPolicyRule

instance Prelude.NFData MetricPolicyRule

instance Prelude.ToJSON MetricPolicyRule where
  toJSON MetricPolicyRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ObjectGroup" Prelude..= objectGroup),
            Prelude.Just
              ("ObjectGroupName" Prelude..= objectGroupName)
          ]
      )
