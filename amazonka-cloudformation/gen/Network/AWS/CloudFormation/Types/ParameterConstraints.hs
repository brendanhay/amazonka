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
-- Module      : Network.AWS.CloudFormation.Types.ParameterConstraints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ParameterConstraints where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A set of criteria that AWS CloudFormation uses to validate parameter
-- values. Although other constraints might be defined in the stack
-- template, AWS CloudFormation returns only the @AllowedValues@ property.
--
-- /See:/ 'newParameterConstraints' smart constructor.
data ParameterConstraints = ParameterConstraints'
  { -- | A list of values that are permitted for a parameter.
    allowedValues :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParameterConstraints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedValues', 'parameterConstraints_allowedValues' - A list of values that are permitted for a parameter.
newParameterConstraints ::
  ParameterConstraints
newParameterConstraints =
  ParameterConstraints' {allowedValues = Core.Nothing}

-- | A list of values that are permitted for a parameter.
parameterConstraints_allowedValues :: Lens.Lens' ParameterConstraints (Core.Maybe [Core.Text])
parameterConstraints_allowedValues = Lens.lens (\ParameterConstraints' {allowedValues} -> allowedValues) (\s@ParameterConstraints' {} a -> s {allowedValues = a} :: ParameterConstraints) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML ParameterConstraints where
  parseXML x =
    ParameterConstraints'
      Core.<$> ( x Core..@? "AllowedValues" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable ParameterConstraints

instance Core.NFData ParameterConstraints
