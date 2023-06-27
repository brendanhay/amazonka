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
-- Module      : Amazonka.EC2.Types.AdditionalDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AdditionalDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AnalysisComponent
import Amazonka.EC2.Types.RuleGroupRuleOptionsPair
import Amazonka.EC2.Types.RuleGroupTypePair
import Amazonka.EC2.Types.RuleOption
import qualified Amazonka.Prelude as Prelude

-- | Describes an additional detail for a path analysis. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc/latest/reachability/additional-detail-codes.html Reachability Analyzer additional detail codes>.
--
-- /See:/ 'newAdditionalDetail' smart constructor.
data AdditionalDetail = AdditionalDetail'
  { -- | The additional detail code.
    additionalDetailType :: Prelude.Maybe Prelude.Text,
    -- | The path component.
    component :: Prelude.Maybe AnalysisComponent,
    -- | The load balancers.
    loadBalancers :: Prelude.Maybe [AnalysisComponent],
    -- | The rule options.
    ruleGroupRuleOptionsPairs :: Prelude.Maybe [RuleGroupRuleOptionsPair],
    -- | The rule group type.
    ruleGroupTypePairs :: Prelude.Maybe [RuleGroupTypePair],
    -- | The rule options.
    ruleOptions :: Prelude.Maybe [RuleOption],
    -- | The name of the VPC endpoint service.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The VPC endpoint service.
    vpcEndpointService :: Prelude.Maybe AnalysisComponent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdditionalDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalDetailType', 'additionalDetail_additionalDetailType' - The additional detail code.
--
-- 'component', 'additionalDetail_component' - The path component.
--
-- 'loadBalancers', 'additionalDetail_loadBalancers' - The load balancers.
--
-- 'ruleGroupRuleOptionsPairs', 'additionalDetail_ruleGroupRuleOptionsPairs' - The rule options.
--
-- 'ruleGroupTypePairs', 'additionalDetail_ruleGroupTypePairs' - The rule group type.
--
-- 'ruleOptions', 'additionalDetail_ruleOptions' - The rule options.
--
-- 'serviceName', 'additionalDetail_serviceName' - The name of the VPC endpoint service.
--
-- 'vpcEndpointService', 'additionalDetail_vpcEndpointService' - The VPC endpoint service.
newAdditionalDetail ::
  AdditionalDetail
newAdditionalDetail =
  AdditionalDetail'
    { additionalDetailType =
        Prelude.Nothing,
      component = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      ruleGroupRuleOptionsPairs = Prelude.Nothing,
      ruleGroupTypePairs = Prelude.Nothing,
      ruleOptions = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      vpcEndpointService = Prelude.Nothing
    }

-- | The additional detail code.
additionalDetail_additionalDetailType :: Lens.Lens' AdditionalDetail (Prelude.Maybe Prelude.Text)
additionalDetail_additionalDetailType = Lens.lens (\AdditionalDetail' {additionalDetailType} -> additionalDetailType) (\s@AdditionalDetail' {} a -> s {additionalDetailType = a} :: AdditionalDetail)

-- | The path component.
additionalDetail_component :: Lens.Lens' AdditionalDetail (Prelude.Maybe AnalysisComponent)
additionalDetail_component = Lens.lens (\AdditionalDetail' {component} -> component) (\s@AdditionalDetail' {} a -> s {component = a} :: AdditionalDetail)

-- | The load balancers.
additionalDetail_loadBalancers :: Lens.Lens' AdditionalDetail (Prelude.Maybe [AnalysisComponent])
additionalDetail_loadBalancers = Lens.lens (\AdditionalDetail' {loadBalancers} -> loadBalancers) (\s@AdditionalDetail' {} a -> s {loadBalancers = a} :: AdditionalDetail) Prelude.. Lens.mapping Lens.coerced

-- | The rule options.
additionalDetail_ruleGroupRuleOptionsPairs :: Lens.Lens' AdditionalDetail (Prelude.Maybe [RuleGroupRuleOptionsPair])
additionalDetail_ruleGroupRuleOptionsPairs = Lens.lens (\AdditionalDetail' {ruleGroupRuleOptionsPairs} -> ruleGroupRuleOptionsPairs) (\s@AdditionalDetail' {} a -> s {ruleGroupRuleOptionsPairs = a} :: AdditionalDetail) Prelude.. Lens.mapping Lens.coerced

-- | The rule group type.
additionalDetail_ruleGroupTypePairs :: Lens.Lens' AdditionalDetail (Prelude.Maybe [RuleGroupTypePair])
additionalDetail_ruleGroupTypePairs = Lens.lens (\AdditionalDetail' {ruleGroupTypePairs} -> ruleGroupTypePairs) (\s@AdditionalDetail' {} a -> s {ruleGroupTypePairs = a} :: AdditionalDetail) Prelude.. Lens.mapping Lens.coerced

-- | The rule options.
additionalDetail_ruleOptions :: Lens.Lens' AdditionalDetail (Prelude.Maybe [RuleOption])
additionalDetail_ruleOptions = Lens.lens (\AdditionalDetail' {ruleOptions} -> ruleOptions) (\s@AdditionalDetail' {} a -> s {ruleOptions = a} :: AdditionalDetail) Prelude.. Lens.mapping Lens.coerced

-- | The name of the VPC endpoint service.
additionalDetail_serviceName :: Lens.Lens' AdditionalDetail (Prelude.Maybe Prelude.Text)
additionalDetail_serviceName = Lens.lens (\AdditionalDetail' {serviceName} -> serviceName) (\s@AdditionalDetail' {} a -> s {serviceName = a} :: AdditionalDetail)

-- | The VPC endpoint service.
additionalDetail_vpcEndpointService :: Lens.Lens' AdditionalDetail (Prelude.Maybe AnalysisComponent)
additionalDetail_vpcEndpointService = Lens.lens (\AdditionalDetail' {vpcEndpointService} -> vpcEndpointService) (\s@AdditionalDetail' {} a -> s {vpcEndpointService = a} :: AdditionalDetail)

instance Data.FromXML AdditionalDetail where
  parseXML x =
    AdditionalDetail'
      Prelude.<$> (x Data..@? "additionalDetailType")
      Prelude.<*> (x Data..@? "component")
      Prelude.<*> ( x
                      Data..@? "loadBalancerSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "ruleGroupRuleOptionsPairSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "ruleGroupTypePairSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "ruleOptionSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "serviceName")
      Prelude.<*> (x Data..@? "vpcEndpointService")

instance Prelude.Hashable AdditionalDetail where
  hashWithSalt _salt AdditionalDetail' {..} =
    _salt
      `Prelude.hashWithSalt` additionalDetailType
      `Prelude.hashWithSalt` component
      `Prelude.hashWithSalt` loadBalancers
      `Prelude.hashWithSalt` ruleGroupRuleOptionsPairs
      `Prelude.hashWithSalt` ruleGroupTypePairs
      `Prelude.hashWithSalt` ruleOptions
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` vpcEndpointService

instance Prelude.NFData AdditionalDetail where
  rnf AdditionalDetail' {..} =
    Prelude.rnf additionalDetailType
      `Prelude.seq` Prelude.rnf component
      `Prelude.seq` Prelude.rnf loadBalancers
      `Prelude.seq` Prelude.rnf ruleGroupRuleOptionsPairs
      `Prelude.seq` Prelude.rnf ruleGroupTypePairs
      `Prelude.seq` Prelude.rnf ruleOptions
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf vpcEndpointService
