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
-- Module      : Amazonka.SecurityHub.Types.AwsWafWebAclDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafWebAclDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafWebAclRule

-- | Details about an WAF WebACL.
--
-- /See:/ 'newAwsWafWebAclDetails' smart constructor.
data AwsWafWebAclDetails = AwsWafWebAclDetails'
  { -- | An array that contains the action for each rule in a WebACL, the
    -- priority of the rule, and the ID of the rule.
    rules :: Prelude.Maybe [AwsWafWebAclRule],
    -- | A unique identifier for a WebACL.
    webAclId :: Prelude.Maybe Prelude.Text,
    -- | A friendly name or description of the WebACL. You can\'t change the name
    -- of a WebACL after you create it.
    name :: Prelude.Maybe Prelude.Text,
    -- | The action to perform if none of the rules contained in the WebACL
    -- match.
    defaultAction :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafWebAclDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'awsWafWebAclDetails_rules' - An array that contains the action for each rule in a WebACL, the
-- priority of the rule, and the ID of the rule.
--
-- 'webAclId', 'awsWafWebAclDetails_webAclId' - A unique identifier for a WebACL.
--
-- 'name', 'awsWafWebAclDetails_name' - A friendly name or description of the WebACL. You can\'t change the name
-- of a WebACL after you create it.
--
-- 'defaultAction', 'awsWafWebAclDetails_defaultAction' - The action to perform if none of the rules contained in the WebACL
-- match.
newAwsWafWebAclDetails ::
  AwsWafWebAclDetails
newAwsWafWebAclDetails =
  AwsWafWebAclDetails'
    { rules = Prelude.Nothing,
      webAclId = Prelude.Nothing,
      name = Prelude.Nothing,
      defaultAction = Prelude.Nothing
    }

-- | An array that contains the action for each rule in a WebACL, the
-- priority of the rule, and the ID of the rule.
awsWafWebAclDetails_rules :: Lens.Lens' AwsWafWebAclDetails (Prelude.Maybe [AwsWafWebAclRule])
awsWafWebAclDetails_rules = Lens.lens (\AwsWafWebAclDetails' {rules} -> rules) (\s@AwsWafWebAclDetails' {} a -> s {rules = a} :: AwsWafWebAclDetails) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for a WebACL.
awsWafWebAclDetails_webAclId :: Lens.Lens' AwsWafWebAclDetails (Prelude.Maybe Prelude.Text)
awsWafWebAclDetails_webAclId = Lens.lens (\AwsWafWebAclDetails' {webAclId} -> webAclId) (\s@AwsWafWebAclDetails' {} a -> s {webAclId = a} :: AwsWafWebAclDetails)

-- | A friendly name or description of the WebACL. You can\'t change the name
-- of a WebACL after you create it.
awsWafWebAclDetails_name :: Lens.Lens' AwsWafWebAclDetails (Prelude.Maybe Prelude.Text)
awsWafWebAclDetails_name = Lens.lens (\AwsWafWebAclDetails' {name} -> name) (\s@AwsWafWebAclDetails' {} a -> s {name = a} :: AwsWafWebAclDetails)

-- | The action to perform if none of the rules contained in the WebACL
-- match.
awsWafWebAclDetails_defaultAction :: Lens.Lens' AwsWafWebAclDetails (Prelude.Maybe Prelude.Text)
awsWafWebAclDetails_defaultAction = Lens.lens (\AwsWafWebAclDetails' {defaultAction} -> defaultAction) (\s@AwsWafWebAclDetails' {} a -> s {defaultAction = a} :: AwsWafWebAclDetails)

instance Core.FromJSON AwsWafWebAclDetails where
  parseJSON =
    Core.withObject
      "AwsWafWebAclDetails"
      ( \x ->
          AwsWafWebAclDetails'
            Prelude.<$> (x Core..:? "Rules" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "WebAclId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DefaultAction")
      )

instance Prelude.Hashable AwsWafWebAclDetails where
  hashWithSalt _salt AwsWafWebAclDetails' {..} =
    _salt `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` webAclId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` defaultAction

instance Prelude.NFData AwsWafWebAclDetails where
  rnf AwsWafWebAclDetails' {..} =
    Prelude.rnf rules
      `Prelude.seq` Prelude.rnf webAclId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf defaultAction

instance Core.ToJSON AwsWafWebAclDetails where
  toJSON AwsWafWebAclDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Rules" Core..=) Prelude.<$> rules,
            ("WebAclId" Core..=) Prelude.<$> webAclId,
            ("Name" Core..=) Prelude.<$> name,
            ("DefaultAction" Core..=) Prelude.<$> defaultAction
          ]
      )
