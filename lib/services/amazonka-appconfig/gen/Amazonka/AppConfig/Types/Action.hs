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
-- Module      : Amazonka.AppConfig.Types.Action
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An action defines the tasks the extension performs during the AppConfig
-- workflow. Each action includes an action point such as
-- @ON_CREATE_HOSTED_CONFIGURATION@, @PRE_DEPLOYMENT@, or @ON_DEPLOYMENT@.
-- Each action also includes a name, a URI to an Lambda function, and an
-- Amazon Resource Name (ARN) for an Identity and Access Management assume
-- role. You specify the name, URI, and ARN for each /action point/ defined
-- in the extension. You can specify the following actions for an
-- extension:
--
-- -   @PRE_CREATE_HOSTED_CONFIGURATION_VERSION@
--
-- -   @PRE_START_DEPLOYMENT@
--
-- -   @ON_DEPLOYMENT_START@
--
-- -   @ON_DEPLOYMENT_STEP@
--
-- -   @ON_DEPLOYMENT_BAKING@
--
-- -   @ON_DEPLOYMENT_COMPLETE@
--
-- -   @ON_DEPLOYMENT_ROLLED_BACK@
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | Information about the action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The action name.
    name :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) for an Identity and Access Management
    -- assume role.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The extension URI associated to the action point in the extension
    -- definition. The URI can be an Amazon Resource Name (ARN) for one of the
    -- following: an Lambda function, an Amazon Simple Queue Service queue, an
    -- Amazon Simple Notification Service topic, or the Amazon EventBridge
    -- default event bus.
    uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'action_description' - Information about the action.
--
-- 'name', 'action_name' - The action name.
--
-- 'roleArn', 'action_roleArn' - An Amazon Resource Name (ARN) for an Identity and Access Management
-- assume role.
--
-- 'uri', 'action_uri' - The extension URI associated to the action point in the extension
-- definition. The URI can be an Amazon Resource Name (ARN) for one of the
-- following: an Lambda function, an Amazon Simple Queue Service queue, an
-- Amazon Simple Notification Service topic, or the Amazon EventBridge
-- default event bus.
newAction ::
  Action
newAction =
  Action'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      uri = Prelude.Nothing
    }

-- | Information about the action.
action_description :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_description = Lens.lens (\Action' {description} -> description) (\s@Action' {} a -> s {description = a} :: Action)

-- | The action name.
action_name :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_name = Lens.lens (\Action' {name} -> name) (\s@Action' {} a -> s {name = a} :: Action)

-- | An Amazon Resource Name (ARN) for an Identity and Access Management
-- assume role.
action_roleArn :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_roleArn = Lens.lens (\Action' {roleArn} -> roleArn) (\s@Action' {} a -> s {roleArn = a} :: Action)

-- | The extension URI associated to the action point in the extension
-- definition. The URI can be an Amazon Resource Name (ARN) for one of the
-- following: an Lambda function, an Amazon Simple Queue Service queue, an
-- Amazon Simple Notification Service topic, or the Amazon EventBridge
-- default event bus.
action_uri :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_uri = Lens.lens (\Action' {uri} -> uri) (\s@Action' {} a -> s {uri = a} :: Action)

instance Data.FromJSON Action where
  parseJSON =
    Data.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "Uri")
      )

instance Prelude.Hashable Action where
  hashWithSalt _salt Action' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` uri

instance Prelude.NFData Action where
  rnf Action' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf uri

instance Data.ToJSON Action where
  toJSON Action' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("Uri" Data..=) Prelude.<$> uri
          ]
      )
