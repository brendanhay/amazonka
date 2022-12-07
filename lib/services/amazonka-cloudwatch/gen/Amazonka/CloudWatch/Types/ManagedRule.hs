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
-- Module      : Amazonka.CloudWatch.Types.ManagedRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.ManagedRule where

import Amazonka.CloudWatch.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the information that\'s required to enable a managed
-- Contributor Insights rule for an Amazon Web Services resource.
--
-- /See:/ 'newManagedRule' smart constructor.
data ManagedRule = ManagedRule'
  { -- | A list of key-value pairs that you can associate with a managed
    -- Contributor Insights rule. You can associate as many as 50 tags with a
    -- rule. Tags can help you organize and categorize your resources. You also
    -- can use them to scope user permissions by granting a user permission to
    -- access or change only the resources that have certain tag values. To
    -- associate tags with a rule, you must have the @cloudwatch:TagResource@
    -- permission in addition to the @cloudwatch:PutInsightRule@ permission. If
    -- you are using this operation to update an existing Contributor Insights
    -- rule, any tags that you specify in this parameter are ignored. To change
    -- the tags of an existing rule, use @TagResource@.
    tags :: Prelude.Maybe [Tag],
    -- | The template name for the managed Contributor Insights rule, as returned
    -- by @ListManagedInsightRules@.
    templateName :: Prelude.Text,
    -- | The ARN of an Amazon Web Services resource that has managed Contributor
    -- Insights rules.
    resourceARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'managedRule_tags' - A list of key-value pairs that you can associate with a managed
-- Contributor Insights rule. You can associate as many as 50 tags with a
-- rule. Tags can help you organize and categorize your resources. You also
-- can use them to scope user permissions by granting a user permission to
-- access or change only the resources that have certain tag values. To
-- associate tags with a rule, you must have the @cloudwatch:TagResource@
-- permission in addition to the @cloudwatch:PutInsightRule@ permission. If
-- you are using this operation to update an existing Contributor Insights
-- rule, any tags that you specify in this parameter are ignored. To change
-- the tags of an existing rule, use @TagResource@.
--
-- 'templateName', 'managedRule_templateName' - The template name for the managed Contributor Insights rule, as returned
-- by @ListManagedInsightRules@.
--
-- 'resourceARN', 'managedRule_resourceARN' - The ARN of an Amazon Web Services resource that has managed Contributor
-- Insights rules.
newManagedRule ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'resourceARN'
  Prelude.Text ->
  ManagedRule
newManagedRule pTemplateName_ pResourceARN_ =
  ManagedRule'
    { tags = Prelude.Nothing,
      templateName = pTemplateName_,
      resourceARN = pResourceARN_
    }

-- | A list of key-value pairs that you can associate with a managed
-- Contributor Insights rule. You can associate as many as 50 tags with a
-- rule. Tags can help you organize and categorize your resources. You also
-- can use them to scope user permissions by granting a user permission to
-- access or change only the resources that have certain tag values. To
-- associate tags with a rule, you must have the @cloudwatch:TagResource@
-- permission in addition to the @cloudwatch:PutInsightRule@ permission. If
-- you are using this operation to update an existing Contributor Insights
-- rule, any tags that you specify in this parameter are ignored. To change
-- the tags of an existing rule, use @TagResource@.
managedRule_tags :: Lens.Lens' ManagedRule (Prelude.Maybe [Tag])
managedRule_tags = Lens.lens (\ManagedRule' {tags} -> tags) (\s@ManagedRule' {} a -> s {tags = a} :: ManagedRule) Prelude.. Lens.mapping Lens.coerced

-- | The template name for the managed Contributor Insights rule, as returned
-- by @ListManagedInsightRules@.
managedRule_templateName :: Lens.Lens' ManagedRule Prelude.Text
managedRule_templateName = Lens.lens (\ManagedRule' {templateName} -> templateName) (\s@ManagedRule' {} a -> s {templateName = a} :: ManagedRule)

-- | The ARN of an Amazon Web Services resource that has managed Contributor
-- Insights rules.
managedRule_resourceARN :: Lens.Lens' ManagedRule Prelude.Text
managedRule_resourceARN = Lens.lens (\ManagedRule' {resourceARN} -> resourceARN) (\s@ManagedRule' {} a -> s {resourceARN = a} :: ManagedRule)

instance Prelude.Hashable ManagedRule where
  hashWithSalt _salt ManagedRule' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` resourceARN

instance Prelude.NFData ManagedRule where
  rnf ManagedRule' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf resourceARN

instance Data.ToQuery ManagedRule where
  toQuery ManagedRule' {..} =
    Prelude.mconcat
      [ "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "TemplateName" Data.=: templateName,
        "ResourceARN" Data.=: resourceARN
      ]
