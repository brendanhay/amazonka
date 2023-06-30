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
-- Module      : Amazonka.Config.Types.Source
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.Source where

import Amazonka.Config.Types.CustomPolicyDetails
import Amazonka.Config.Types.Owner
import Amazonka.Config.Types.SourceDetail
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the CustomPolicyDetails, the rule owner (@Amazon Web Services@
-- for managed rules, @CUSTOM_POLICY@ for Custom Policy rules, and
-- @CUSTOM_LAMBDA@ for Custom Lambda rules), the rule identifier, and the
-- events that cause the evaluation of your Amazon Web Services resources.
--
-- /See:/ 'newSource' smart constructor.
data Source = Source'
  { -- | Provides the runtime system, policy definition, and whether debug
    -- logging is enabled. Required when owner is set to @CUSTOM_POLICY@.
    customPolicyDetails :: Prelude.Maybe CustomPolicyDetails,
    -- | Provides the source and the message types that cause Config to evaluate
    -- your Amazon Web Services resources against a rule. It also provides the
    -- frequency with which you want Config to run evaluations for the rule if
    -- the trigger type is periodic.
    --
    -- If the owner is set to @CUSTOM_POLICY@, the only acceptable values for
    -- the Config rule trigger message type are
    -- @ConfigurationItemChangeNotification@ and
    -- @OversizedConfigurationItemChangeNotification@.
    sourceDetails :: Prelude.Maybe [SourceDetail],
    -- | For Config Managed rules, a predefined identifier from a list. For
    -- example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed
    -- rule, see
    -- <https://docs.aws.amazon.com/config/latest/developerguide/managed-rules-by-aws-config.html List of Config Managed Rules>.
    --
    -- For Config Custom Lambda rules, the identifier is the Amazon Resource
    -- Name (ARN) of the rule\'s Lambda function, such as
    -- @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@.
    --
    -- For Config Custom Policy rules, this field will be ignored.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Amazon Web Services or the customer owns and manages
    -- the Config rule.
    --
    -- Config Managed Rules are predefined rules owned by Amazon Web Services.
    -- For more information, see
    -- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Config Managed Rules>
    -- in the /Config developer guide/.
    --
    -- Config Custom Rules are rules that you can develop either with Guard
    -- (@CUSTOM_POLICY@) or Lambda (@CUSTOM_LAMBDA@). For more information, see
    -- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_develop-rules.html Config Custom Rules>
    -- in the /Config developer guide/.
    owner :: Owner
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Source' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customPolicyDetails', 'source_customPolicyDetails' - Provides the runtime system, policy definition, and whether debug
-- logging is enabled. Required when owner is set to @CUSTOM_POLICY@.
--
-- 'sourceDetails', 'source_sourceDetails' - Provides the source and the message types that cause Config to evaluate
-- your Amazon Web Services resources against a rule. It also provides the
-- frequency with which you want Config to run evaluations for the rule if
-- the trigger type is periodic.
--
-- If the owner is set to @CUSTOM_POLICY@, the only acceptable values for
-- the Config rule trigger message type are
-- @ConfigurationItemChangeNotification@ and
-- @OversizedConfigurationItemChangeNotification@.
--
-- 'sourceIdentifier', 'source_sourceIdentifier' - For Config Managed rules, a predefined identifier from a list. For
-- example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed
-- rule, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/managed-rules-by-aws-config.html List of Config Managed Rules>.
--
-- For Config Custom Lambda rules, the identifier is the Amazon Resource
-- Name (ARN) of the rule\'s Lambda function, such as
-- @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@.
--
-- For Config Custom Policy rules, this field will be ignored.
--
-- 'owner', 'source_owner' - Indicates whether Amazon Web Services or the customer owns and manages
-- the Config rule.
--
-- Config Managed Rules are predefined rules owned by Amazon Web Services.
-- For more information, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Config Managed Rules>
-- in the /Config developer guide/.
--
-- Config Custom Rules are rules that you can develop either with Guard
-- (@CUSTOM_POLICY@) or Lambda (@CUSTOM_LAMBDA@). For more information, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_develop-rules.html Config Custom Rules>
-- in the /Config developer guide/.
newSource ::
  -- | 'owner'
  Owner ->
  Source
newSource pOwner_ =
  Source'
    { customPolicyDetails = Prelude.Nothing,
      sourceDetails = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      owner = pOwner_
    }

-- | Provides the runtime system, policy definition, and whether debug
-- logging is enabled. Required when owner is set to @CUSTOM_POLICY@.
source_customPolicyDetails :: Lens.Lens' Source (Prelude.Maybe CustomPolicyDetails)
source_customPolicyDetails = Lens.lens (\Source' {customPolicyDetails} -> customPolicyDetails) (\s@Source' {} a -> s {customPolicyDetails = a} :: Source)

-- | Provides the source and the message types that cause Config to evaluate
-- your Amazon Web Services resources against a rule. It also provides the
-- frequency with which you want Config to run evaluations for the rule if
-- the trigger type is periodic.
--
-- If the owner is set to @CUSTOM_POLICY@, the only acceptable values for
-- the Config rule trigger message type are
-- @ConfigurationItemChangeNotification@ and
-- @OversizedConfigurationItemChangeNotification@.
source_sourceDetails :: Lens.Lens' Source (Prelude.Maybe [SourceDetail])
source_sourceDetails = Lens.lens (\Source' {sourceDetails} -> sourceDetails) (\s@Source' {} a -> s {sourceDetails = a} :: Source) Prelude.. Lens.mapping Lens.coerced

-- | For Config Managed rules, a predefined identifier from a list. For
-- example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed
-- rule, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/managed-rules-by-aws-config.html List of Config Managed Rules>.
--
-- For Config Custom Lambda rules, the identifier is the Amazon Resource
-- Name (ARN) of the rule\'s Lambda function, such as
-- @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@.
--
-- For Config Custom Policy rules, this field will be ignored.
source_sourceIdentifier :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_sourceIdentifier = Lens.lens (\Source' {sourceIdentifier} -> sourceIdentifier) (\s@Source' {} a -> s {sourceIdentifier = a} :: Source)

-- | Indicates whether Amazon Web Services or the customer owns and manages
-- the Config rule.
--
-- Config Managed Rules are predefined rules owned by Amazon Web Services.
-- For more information, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Config Managed Rules>
-- in the /Config developer guide/.
--
-- Config Custom Rules are rules that you can develop either with Guard
-- (@CUSTOM_POLICY@) or Lambda (@CUSTOM_LAMBDA@). For more information, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_develop-rules.html Config Custom Rules>
-- in the /Config developer guide/.
source_owner :: Lens.Lens' Source Owner
source_owner = Lens.lens (\Source' {owner} -> owner) (\s@Source' {} a -> s {owner = a} :: Source)

instance Data.FromJSON Source where
  parseJSON =
    Data.withObject
      "Source"
      ( \x ->
          Source'
            Prelude.<$> (x Data..:? "CustomPolicyDetails")
            Prelude.<*> (x Data..:? "SourceDetails" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SourceIdentifier")
            Prelude.<*> (x Data..: "Owner")
      )

instance Prelude.Hashable Source where
  hashWithSalt _salt Source' {..} =
    _salt
      `Prelude.hashWithSalt` customPolicyDetails
      `Prelude.hashWithSalt` sourceDetails
      `Prelude.hashWithSalt` sourceIdentifier
      `Prelude.hashWithSalt` owner

instance Prelude.NFData Source where
  rnf Source' {..} =
    Prelude.rnf customPolicyDetails
      `Prelude.seq` Prelude.rnf sourceDetails
      `Prelude.seq` Prelude.rnf sourceIdentifier
      `Prelude.seq` Prelude.rnf owner

instance Data.ToJSON Source where
  toJSON Source' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomPolicyDetails" Data..=)
              Prelude.<$> customPolicyDetails,
            ("SourceDetails" Data..=) Prelude.<$> sourceDetails,
            ("SourceIdentifier" Data..=)
              Prelude.<$> sourceIdentifier,
            Prelude.Just ("Owner" Data..= owner)
          ]
      )
