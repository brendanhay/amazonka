{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the solution stack.
--
-- /See:/ 'newSolutionStackDescription' smart constructor.
data SolutionStackDescription = SolutionStackDescription'
  { -- | The permitted file types allowed for a solution stack.
    permittedFileTypes :: Prelude.Maybe [Prelude.Text],
    -- | The name of the solution stack.
    solutionStackName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      solutionStackName = Prelude.Nothing
    }

-- | The permitted file types allowed for a solution stack.
solutionStackDescription_permittedFileTypes :: Lens.Lens' SolutionStackDescription (Prelude.Maybe [Prelude.Text])
solutionStackDescription_permittedFileTypes = Lens.lens (\SolutionStackDescription' {permittedFileTypes} -> permittedFileTypes) (\s@SolutionStackDescription' {} a -> s {permittedFileTypes = a} :: SolutionStackDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the solution stack.
solutionStackDescription_solutionStackName :: Lens.Lens' SolutionStackDescription (Prelude.Maybe Prelude.Text)
solutionStackDescription_solutionStackName = Lens.lens (\SolutionStackDescription' {solutionStackName} -> solutionStackName) (\s@SolutionStackDescription' {} a -> s {solutionStackName = a} :: SolutionStackDescription)

instance Prelude.FromXML SolutionStackDescription where
  parseXML x =
    SolutionStackDescription'
      Prelude.<$> ( x Prelude..@? "PermittedFileTypes"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "SolutionStackName")

instance Prelude.Hashable SolutionStackDescription

instance Prelude.NFData SolutionStackDescription
