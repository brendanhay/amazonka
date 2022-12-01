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
-- Module      : Amazonka.SecurityHub.Types.AwsWafRegionalWebAclDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafRegionalWebAclDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListDetails

-- | Provides information about the web access control list (web ACL). The
-- web ACL contains the rules that identify the requests that you want to
-- allow, block, or count.
--
-- /See:/ 'newAwsWafRegionalWebAclDetails' smart constructor.
data AwsWafRegionalWebAclDetails = AwsWafRegionalWebAclDetails'
  { -- | A descriptive name for the web ACL.
    name :: Prelude.Maybe Prelude.Text,
    -- | An array that contains the action for each rule in a web ACL, the
    -- priority of the rule, and the ID of the rule.
    rulesList :: Prelude.Maybe [AwsWafRegionalWebAclRulesListDetails],
    -- | The ID of the web ACL.
    webAclId :: Prelude.Maybe Prelude.Text,
    -- | A name for the metrics for this web ACL.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The action to perform if none of the rules contained in the web ACL
    -- match.
    defaultAction :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafRegionalWebAclDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsWafRegionalWebAclDetails_name' - A descriptive name for the web ACL.
--
-- 'rulesList', 'awsWafRegionalWebAclDetails_rulesList' - An array that contains the action for each rule in a web ACL, the
-- priority of the rule, and the ID of the rule.
--
-- 'webAclId', 'awsWafRegionalWebAclDetails_webAclId' - The ID of the web ACL.
--
-- 'metricName', 'awsWafRegionalWebAclDetails_metricName' - A name for the metrics for this web ACL.
--
-- 'defaultAction', 'awsWafRegionalWebAclDetails_defaultAction' - The action to perform if none of the rules contained in the web ACL
-- match.
newAwsWafRegionalWebAclDetails ::
  AwsWafRegionalWebAclDetails
newAwsWafRegionalWebAclDetails =
  AwsWafRegionalWebAclDetails'
    { name =
        Prelude.Nothing,
      rulesList = Prelude.Nothing,
      webAclId = Prelude.Nothing,
      metricName = Prelude.Nothing,
      defaultAction = Prelude.Nothing
    }

-- | A descriptive name for the web ACL.
awsWafRegionalWebAclDetails_name :: Lens.Lens' AwsWafRegionalWebAclDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalWebAclDetails_name = Lens.lens (\AwsWafRegionalWebAclDetails' {name} -> name) (\s@AwsWafRegionalWebAclDetails' {} a -> s {name = a} :: AwsWafRegionalWebAclDetails)

-- | An array that contains the action for each rule in a web ACL, the
-- priority of the rule, and the ID of the rule.
awsWafRegionalWebAclDetails_rulesList :: Lens.Lens' AwsWafRegionalWebAclDetails (Prelude.Maybe [AwsWafRegionalWebAclRulesListDetails])
awsWafRegionalWebAclDetails_rulesList = Lens.lens (\AwsWafRegionalWebAclDetails' {rulesList} -> rulesList) (\s@AwsWafRegionalWebAclDetails' {} a -> s {rulesList = a} :: AwsWafRegionalWebAclDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the web ACL.
awsWafRegionalWebAclDetails_webAclId :: Lens.Lens' AwsWafRegionalWebAclDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalWebAclDetails_webAclId = Lens.lens (\AwsWafRegionalWebAclDetails' {webAclId} -> webAclId) (\s@AwsWafRegionalWebAclDetails' {} a -> s {webAclId = a} :: AwsWafRegionalWebAclDetails)

-- | A name for the metrics for this web ACL.
awsWafRegionalWebAclDetails_metricName :: Lens.Lens' AwsWafRegionalWebAclDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalWebAclDetails_metricName = Lens.lens (\AwsWafRegionalWebAclDetails' {metricName} -> metricName) (\s@AwsWafRegionalWebAclDetails' {} a -> s {metricName = a} :: AwsWafRegionalWebAclDetails)

-- | The action to perform if none of the rules contained in the web ACL
-- match.
awsWafRegionalWebAclDetails_defaultAction :: Lens.Lens' AwsWafRegionalWebAclDetails (Prelude.Maybe Prelude.Text)
awsWafRegionalWebAclDetails_defaultAction = Lens.lens (\AwsWafRegionalWebAclDetails' {defaultAction} -> defaultAction) (\s@AwsWafRegionalWebAclDetails' {} a -> s {defaultAction = a} :: AwsWafRegionalWebAclDetails)

instance Core.FromJSON AwsWafRegionalWebAclDetails where
  parseJSON =
    Core.withObject
      "AwsWafRegionalWebAclDetails"
      ( \x ->
          AwsWafRegionalWebAclDetails'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "RulesList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "WebAclId")
            Prelude.<*> (x Core..:? "MetricName")
            Prelude.<*> (x Core..:? "DefaultAction")
      )

instance Prelude.Hashable AwsWafRegionalWebAclDetails where
  hashWithSalt _salt AwsWafRegionalWebAclDetails' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` rulesList
      `Prelude.hashWithSalt` webAclId
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` defaultAction

instance Prelude.NFData AwsWafRegionalWebAclDetails where
  rnf AwsWafRegionalWebAclDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf rulesList
      `Prelude.seq` Prelude.rnf webAclId
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf defaultAction

instance Core.ToJSON AwsWafRegionalWebAclDetails where
  toJSON AwsWafRegionalWebAclDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("RulesList" Core..=) Prelude.<$> rulesList,
            ("WebAclId" Core..=) Prelude.<$> webAclId,
            ("MetricName" Core..=) Prelude.<$> metricName,
            ("DefaultAction" Core..=) Prelude.<$> defaultAction
          ]
      )
