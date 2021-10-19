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
-- Module      : Network.AWS.Config.Types.Source
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Source where

import Network.AWS.Config.Types.Owner
import Network.AWS.Config.Types.SourceDetail
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the Config rule owner (Amazon Web Services or customer), the
-- rule identifier, and the events that trigger the evaluation of your
-- Amazon Web Services resources.
--
-- /See:/ 'newSource' smart constructor.
data Source = Source'
  { -- | Provides the source and type of the event that causes Config to evaluate
    -- your Amazon Web Services resources.
    sourceDetails :: Prelude.Maybe [SourceDetail],
    -- | Indicates whether Amazon Web Services or the customer owns and manages
    -- the Config rule.
    owner :: Owner,
    -- | For Config managed rules, a predefined identifier from a list. For
    -- example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed
    -- rule, see
    -- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using Config managed rules>.
    --
    -- For custom rules, the identifier is the Amazon Resource Name (ARN) of
    -- the rule\'s Lambda function, such as
    -- @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@.
    sourceIdentifier :: Prelude.Text
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
-- 'sourceDetails', 'source_sourceDetails' - Provides the source and type of the event that causes Config to evaluate
-- your Amazon Web Services resources.
--
-- 'owner', 'source_owner' - Indicates whether Amazon Web Services or the customer owns and manages
-- the Config rule.
--
-- 'sourceIdentifier', 'source_sourceIdentifier' - For Config managed rules, a predefined identifier from a list. For
-- example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed
-- rule, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using Config managed rules>.
--
-- For custom rules, the identifier is the Amazon Resource Name (ARN) of
-- the rule\'s Lambda function, such as
-- @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@.
newSource ::
  -- | 'owner'
  Owner ->
  -- | 'sourceIdentifier'
  Prelude.Text ->
  Source
newSource pOwner_ pSourceIdentifier_ =
  Source'
    { sourceDetails = Prelude.Nothing,
      owner = pOwner_,
      sourceIdentifier = pSourceIdentifier_
    }

-- | Provides the source and type of the event that causes Config to evaluate
-- your Amazon Web Services resources.
source_sourceDetails :: Lens.Lens' Source (Prelude.Maybe [SourceDetail])
source_sourceDetails = Lens.lens (\Source' {sourceDetails} -> sourceDetails) (\s@Source' {} a -> s {sourceDetails = a} :: Source) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether Amazon Web Services or the customer owns and manages
-- the Config rule.
source_owner :: Lens.Lens' Source Owner
source_owner = Lens.lens (\Source' {owner} -> owner) (\s@Source' {} a -> s {owner = a} :: Source)

-- | For Config managed rules, a predefined identifier from a list. For
-- example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed
-- rule, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using Config managed rules>.
--
-- For custom rules, the identifier is the Amazon Resource Name (ARN) of
-- the rule\'s Lambda function, such as
-- @arn:aws:lambda:us-east-2:123456789012:function:custom_rule_name@.
source_sourceIdentifier :: Lens.Lens' Source Prelude.Text
source_sourceIdentifier = Lens.lens (\Source' {sourceIdentifier} -> sourceIdentifier) (\s@Source' {} a -> s {sourceIdentifier = a} :: Source)

instance Core.FromJSON Source where
  parseJSON =
    Core.withObject
      "Source"
      ( \x ->
          Source'
            Prelude.<$> (x Core..:? "SourceDetails" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Owner")
            Prelude.<*> (x Core..: "SourceIdentifier")
      )

instance Prelude.Hashable Source

instance Prelude.NFData Source

instance Core.ToJSON Source where
  toJSON Source' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceDetails" Core..=) Prelude.<$> sourceDetails,
            Prelude.Just ("Owner" Core..= owner),
            Prelude.Just
              ("SourceIdentifier" Core..= sourceIdentifier)
          ]
      )
