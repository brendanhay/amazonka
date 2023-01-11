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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafWebAclDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsWafWebAclRule

-- | Provides information about an WAF web access control list (web ACL).
--
-- /See:/ 'newAwsWafWebAclDetails' smart constructor.
data AwsWafWebAclDetails = AwsWafWebAclDetails'
  { -- | The action to perform if none of the rules contained in the web ACL
    -- match.
    defaultAction :: Prelude.Maybe Prelude.Text,
    -- | A friendly name or description of the web ACL. You can\'t change the
    -- name of a web ACL after you create it.
    name :: Prelude.Maybe Prelude.Text,
    -- | An array that contains the action for each rule in a web ACL, the
    -- priority of the rule, and the ID of the rule.
    rules :: Prelude.Maybe [AwsWafWebAclRule],
    -- | A unique identifier for a web ACL.
    webAclId :: Prelude.Maybe Prelude.Text
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
-- 'defaultAction', 'awsWafWebAclDetails_defaultAction' - The action to perform if none of the rules contained in the web ACL
-- match.
--
-- 'name', 'awsWafWebAclDetails_name' - A friendly name or description of the web ACL. You can\'t change the
-- name of a web ACL after you create it.
--
-- 'rules', 'awsWafWebAclDetails_rules' - An array that contains the action for each rule in a web ACL, the
-- priority of the rule, and the ID of the rule.
--
-- 'webAclId', 'awsWafWebAclDetails_webAclId' - A unique identifier for a web ACL.
newAwsWafWebAclDetails ::
  AwsWafWebAclDetails
newAwsWafWebAclDetails =
  AwsWafWebAclDetails'
    { defaultAction =
        Prelude.Nothing,
      name = Prelude.Nothing,
      rules = Prelude.Nothing,
      webAclId = Prelude.Nothing
    }

-- | The action to perform if none of the rules contained in the web ACL
-- match.
awsWafWebAclDetails_defaultAction :: Lens.Lens' AwsWafWebAclDetails (Prelude.Maybe Prelude.Text)
awsWafWebAclDetails_defaultAction = Lens.lens (\AwsWafWebAclDetails' {defaultAction} -> defaultAction) (\s@AwsWafWebAclDetails' {} a -> s {defaultAction = a} :: AwsWafWebAclDetails)

-- | A friendly name or description of the web ACL. You can\'t change the
-- name of a web ACL after you create it.
awsWafWebAclDetails_name :: Lens.Lens' AwsWafWebAclDetails (Prelude.Maybe Prelude.Text)
awsWafWebAclDetails_name = Lens.lens (\AwsWafWebAclDetails' {name} -> name) (\s@AwsWafWebAclDetails' {} a -> s {name = a} :: AwsWafWebAclDetails)

-- | An array that contains the action for each rule in a web ACL, the
-- priority of the rule, and the ID of the rule.
awsWafWebAclDetails_rules :: Lens.Lens' AwsWafWebAclDetails (Prelude.Maybe [AwsWafWebAclRule])
awsWafWebAclDetails_rules = Lens.lens (\AwsWafWebAclDetails' {rules} -> rules) (\s@AwsWafWebAclDetails' {} a -> s {rules = a} :: AwsWafWebAclDetails) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for a web ACL.
awsWafWebAclDetails_webAclId :: Lens.Lens' AwsWafWebAclDetails (Prelude.Maybe Prelude.Text)
awsWafWebAclDetails_webAclId = Lens.lens (\AwsWafWebAclDetails' {webAclId} -> webAclId) (\s@AwsWafWebAclDetails' {} a -> s {webAclId = a} :: AwsWafWebAclDetails)

instance Data.FromJSON AwsWafWebAclDetails where
  parseJSON =
    Data.withObject
      "AwsWafWebAclDetails"
      ( \x ->
          AwsWafWebAclDetails'
            Prelude.<$> (x Data..:? "DefaultAction")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Rules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "WebAclId")
      )

instance Prelude.Hashable AwsWafWebAclDetails where
  hashWithSalt _salt AwsWafWebAclDetails' {..} =
    _salt `Prelude.hashWithSalt` defaultAction
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` webAclId

instance Prelude.NFData AwsWafWebAclDetails where
  rnf AwsWafWebAclDetails' {..} =
    Prelude.rnf defaultAction
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf webAclId

instance Data.ToJSON AwsWafWebAclDetails where
  toJSON AwsWafWebAclDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultAction" Data..=) Prelude.<$> defaultAction,
            ("Name" Data..=) Prelude.<$> name,
            ("Rules" Data..=) Prelude.<$> rules,
            ("WebAclId" Data..=) Prelude.<$> webAclId
          ]
      )
