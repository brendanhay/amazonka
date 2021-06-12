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
-- Module      : Network.AWS.ElasticBeanstalk.Types.SolutionStackDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SolutionStackDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the solution stack.
--
-- /See:/ 'newSolutionStackDescription' smart constructor.
data SolutionStackDescription = SolutionStackDescription'
  { -- | The permitted file types allowed for a solution stack.
    permittedFileTypes :: Core.Maybe [Core.Text],
    -- | The name of the solution stack.
    solutionStackName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SolutionStackDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permittedFileTypes', 'solutionStackDescription_permittedFileTypes' - The permitted file types allowed for a solution stack.
--
-- 'solutionStackName', 'solutionStackDescription_solutionStackName' - The name of the solution stack.
newSolutionStackDescription ::
  SolutionStackDescription
newSolutionStackDescription =
  SolutionStackDescription'
    { permittedFileTypes =
        Core.Nothing,
      solutionStackName = Core.Nothing
    }

-- | The permitted file types allowed for a solution stack.
solutionStackDescription_permittedFileTypes :: Lens.Lens' SolutionStackDescription (Core.Maybe [Core.Text])
solutionStackDescription_permittedFileTypes = Lens.lens (\SolutionStackDescription' {permittedFileTypes} -> permittedFileTypes) (\s@SolutionStackDescription' {} a -> s {permittedFileTypes = a} :: SolutionStackDescription) Core.. Lens.mapping Lens._Coerce

-- | The name of the solution stack.
solutionStackDescription_solutionStackName :: Lens.Lens' SolutionStackDescription (Core.Maybe Core.Text)
solutionStackDescription_solutionStackName = Lens.lens (\SolutionStackDescription' {solutionStackName} -> solutionStackName) (\s@SolutionStackDescription' {} a -> s {solutionStackName = a} :: SolutionStackDescription)

instance Core.FromXML SolutionStackDescription where
  parseXML x =
    SolutionStackDescription'
      Core.<$> ( x Core..@? "PermittedFileTypes" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "SolutionStackName")

instance Core.Hashable SolutionStackDescription

instance Core.NFData SolutionStackDescription
