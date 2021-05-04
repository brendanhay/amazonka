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
-- Module      : Network.AWS.IAM.Types.GetContextKeysForPolicyResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.GetContextKeysForPolicyResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the response to a successful GetContextKeysForPrincipalPolicy
-- or GetContextKeysForCustomPolicy request.
--
-- /See:/ 'newGetContextKeysForPolicyResponse' smart constructor.
data GetContextKeysForPolicyResponse = GetContextKeysForPolicyResponse'
  { -- | The list of context keys that are referenced in the input policies.
    contextKeyNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetContextKeysForPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextKeyNames', 'getContextKeysForPolicyResponse_contextKeyNames' - The list of context keys that are referenced in the input policies.
newGetContextKeysForPolicyResponse ::
  GetContextKeysForPolicyResponse
newGetContextKeysForPolicyResponse =
  GetContextKeysForPolicyResponse'
    { contextKeyNames =
        Prelude.Nothing
    }

-- | The list of context keys that are referenced in the input policies.
getContextKeysForPolicyResponse_contextKeyNames :: Lens.Lens' GetContextKeysForPolicyResponse (Prelude.Maybe [Prelude.Text])
getContextKeysForPolicyResponse_contextKeyNames = Lens.lens (\GetContextKeysForPolicyResponse' {contextKeyNames} -> contextKeyNames) (\s@GetContextKeysForPolicyResponse' {} a -> s {contextKeyNames = a} :: GetContextKeysForPolicyResponse) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromXML
    GetContextKeysForPolicyResponse
  where
  parseXML x =
    GetContextKeysForPolicyResponse'
      Prelude.<$> ( x Prelude..@? "ContextKeyNames"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance
  Prelude.Hashable
    GetContextKeysForPolicyResponse

instance
  Prelude.NFData
    GetContextKeysForPolicyResponse
