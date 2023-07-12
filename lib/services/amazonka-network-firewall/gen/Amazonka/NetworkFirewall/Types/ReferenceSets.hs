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
-- Module      : Amazonka.NetworkFirewall.Types.ReferenceSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.ReferenceSets where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.IPSetReference
import qualified Amazonka.Prelude as Prelude

-- | Contains a set of IP set references.
--
-- /See:/ 'newReferenceSets' smart constructor.
data ReferenceSets = ReferenceSets'
  { -- | The list of IP set references.
    iPSetReferences :: Prelude.Maybe (Prelude.HashMap Prelude.Text IPSetReference)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPSetReferences', 'referenceSets_iPSetReferences' - The list of IP set references.
newReferenceSets ::
  ReferenceSets
newReferenceSets =
  ReferenceSets' {iPSetReferences = Prelude.Nothing}

-- | The list of IP set references.
referenceSets_iPSetReferences :: Lens.Lens' ReferenceSets (Prelude.Maybe (Prelude.HashMap Prelude.Text IPSetReference))
referenceSets_iPSetReferences = Lens.lens (\ReferenceSets' {iPSetReferences} -> iPSetReferences) (\s@ReferenceSets' {} a -> s {iPSetReferences = a} :: ReferenceSets) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ReferenceSets where
  parseJSON =
    Data.withObject
      "ReferenceSets"
      ( \x ->
          ReferenceSets'
            Prelude.<$> ( x
                            Data..:? "IPSetReferences"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ReferenceSets where
  hashWithSalt _salt ReferenceSets' {..} =
    _salt `Prelude.hashWithSalt` iPSetReferences

instance Prelude.NFData ReferenceSets where
  rnf ReferenceSets' {..} = Prelude.rnf iPSetReferences

instance Data.ToJSON ReferenceSets where
  toJSON ReferenceSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IPSetReferences" Data..=)
              Prelude.<$> iPSetReferences
          ]
      )
