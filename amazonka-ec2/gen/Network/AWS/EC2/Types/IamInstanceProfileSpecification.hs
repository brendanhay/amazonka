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
-- Module      : Network.AWS.EC2.Types.IamInstanceProfileSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IamInstanceProfileSpecification where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an IAM instance profile.
--
-- /See:/ 'newIamInstanceProfileSpecification' smart constructor.
data IamInstanceProfileSpecification = IamInstanceProfileSpecification'
  { -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the instance profile.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IamInstanceProfileSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'iamInstanceProfileSpecification_arn' - The Amazon Resource Name (ARN) of the instance profile.
--
-- 'name', 'iamInstanceProfileSpecification_name' - The name of the instance profile.
newIamInstanceProfileSpecification ::
  IamInstanceProfileSpecification
newIamInstanceProfileSpecification =
  IamInstanceProfileSpecification'
    { arn =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
iamInstanceProfileSpecification_arn :: Lens.Lens' IamInstanceProfileSpecification (Prelude.Maybe Prelude.Text)
iamInstanceProfileSpecification_arn = Lens.lens (\IamInstanceProfileSpecification' {arn} -> arn) (\s@IamInstanceProfileSpecification' {} a -> s {arn = a} :: IamInstanceProfileSpecification)

-- | The name of the instance profile.
iamInstanceProfileSpecification_name :: Lens.Lens' IamInstanceProfileSpecification (Prelude.Maybe Prelude.Text)
iamInstanceProfileSpecification_name = Lens.lens (\IamInstanceProfileSpecification' {name} -> name) (\s@IamInstanceProfileSpecification' {} a -> s {name = a} :: IamInstanceProfileSpecification)

instance
  Prelude.FromXML
    IamInstanceProfileSpecification
  where
  parseXML x =
    IamInstanceProfileSpecification'
      Prelude.<$> (x Prelude..@? "arn")
      Prelude.<*> (x Prelude..@? "name")

instance
  Prelude.Hashable
    IamInstanceProfileSpecification

instance
  Prelude.NFData
    IamInstanceProfileSpecification

instance
  Prelude.ToQuery
    IamInstanceProfileSpecification
  where
  toQuery IamInstanceProfileSpecification' {..} =
    Prelude.mconcat
      ["Arn" Prelude.=: arn, "Name" Prelude.=: name]
