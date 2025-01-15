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
-- Module      : Amazonka.IoTWireless.Types.Destinations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.Destinations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.ExpressionType
import qualified Amazonka.Prelude as Prelude

-- | Describes a destination.
--
-- /See:/ 'newDestinations' smart constructor.
data Destinations = Destinations'
  { -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The rule name or topic rule to send messages to.
    expression :: Prelude.Maybe Prelude.Text,
    -- | The type of value in @Expression@.
    expressionType :: Prelude.Maybe ExpressionType,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM Role that authorizes the destination.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Destinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'destinations_arn' - The Amazon Resource Name of the resource.
--
-- 'description', 'destinations_description' - The description of the resource.
--
-- 'expression', 'destinations_expression' - The rule name or topic rule to send messages to.
--
-- 'expressionType', 'destinations_expressionType' - The type of value in @Expression@.
--
-- 'name', 'destinations_name' - The name of the resource.
--
-- 'roleArn', 'destinations_roleArn' - The ARN of the IAM Role that authorizes the destination.
newDestinations ::
  Destinations
newDestinations =
  Destinations'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      expression = Prelude.Nothing,
      expressionType = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The Amazon Resource Name of the resource.
destinations_arn :: Lens.Lens' Destinations (Prelude.Maybe Prelude.Text)
destinations_arn = Lens.lens (\Destinations' {arn} -> arn) (\s@Destinations' {} a -> s {arn = a} :: Destinations)

-- | The description of the resource.
destinations_description :: Lens.Lens' Destinations (Prelude.Maybe Prelude.Text)
destinations_description = Lens.lens (\Destinations' {description} -> description) (\s@Destinations' {} a -> s {description = a} :: Destinations)

-- | The rule name or topic rule to send messages to.
destinations_expression :: Lens.Lens' Destinations (Prelude.Maybe Prelude.Text)
destinations_expression = Lens.lens (\Destinations' {expression} -> expression) (\s@Destinations' {} a -> s {expression = a} :: Destinations)

-- | The type of value in @Expression@.
destinations_expressionType :: Lens.Lens' Destinations (Prelude.Maybe ExpressionType)
destinations_expressionType = Lens.lens (\Destinations' {expressionType} -> expressionType) (\s@Destinations' {} a -> s {expressionType = a} :: Destinations)

-- | The name of the resource.
destinations_name :: Lens.Lens' Destinations (Prelude.Maybe Prelude.Text)
destinations_name = Lens.lens (\Destinations' {name} -> name) (\s@Destinations' {} a -> s {name = a} :: Destinations)

-- | The ARN of the IAM Role that authorizes the destination.
destinations_roleArn :: Lens.Lens' Destinations (Prelude.Maybe Prelude.Text)
destinations_roleArn = Lens.lens (\Destinations' {roleArn} -> roleArn) (\s@Destinations' {} a -> s {roleArn = a} :: Destinations)

instance Data.FromJSON Destinations where
  parseJSON =
    Data.withObject
      "Destinations"
      ( \x ->
          Destinations'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Expression")
            Prelude.<*> (x Data..:? "ExpressionType")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RoleArn")
      )

instance Prelude.Hashable Destinations where
  hashWithSalt _salt Destinations' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` expressionType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData Destinations where
  rnf Destinations' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf expression `Prelude.seq`
          Prelude.rnf expressionType `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf roleArn
