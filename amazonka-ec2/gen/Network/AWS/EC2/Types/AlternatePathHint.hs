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
-- Module      : Network.AWS.EC2.Types.AlternatePathHint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AlternatePathHint where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an potential intermediate component of a feasible path.
--
-- /See:/ 'newAlternatePathHint' smart constructor.
data AlternatePathHint = AlternatePathHint'
  { -- | The ID of the component.
    componentId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the component.
    componentArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AlternatePathHint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentId', 'alternatePathHint_componentId' - The ID of the component.
--
-- 'componentArn', 'alternatePathHint_componentArn' - The Amazon Resource Name (ARN) of the component.
newAlternatePathHint ::
  AlternatePathHint
newAlternatePathHint =
  AlternatePathHint'
    { componentId = Prelude.Nothing,
      componentArn = Prelude.Nothing
    }

-- | The ID of the component.
alternatePathHint_componentId :: Lens.Lens' AlternatePathHint (Prelude.Maybe Prelude.Text)
alternatePathHint_componentId = Lens.lens (\AlternatePathHint' {componentId} -> componentId) (\s@AlternatePathHint' {} a -> s {componentId = a} :: AlternatePathHint)

-- | The Amazon Resource Name (ARN) of the component.
alternatePathHint_componentArn :: Lens.Lens' AlternatePathHint (Prelude.Maybe Prelude.Text)
alternatePathHint_componentArn = Lens.lens (\AlternatePathHint' {componentArn} -> componentArn) (\s@AlternatePathHint' {} a -> s {componentArn = a} :: AlternatePathHint)

instance Prelude.FromXML AlternatePathHint where
  parseXML x =
    AlternatePathHint'
      Prelude.<$> (x Prelude..@? "componentId")
      Prelude.<*> (x Prelude..@? "componentArn")

instance Prelude.Hashable AlternatePathHint

instance Prelude.NFData AlternatePathHint
