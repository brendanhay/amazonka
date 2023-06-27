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
-- Module      : Amazonka.WAFRegional.Types.IPSetUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFRegional.Types.IPSetUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFRegional.Types.ChangeAction
import Amazonka.WAFRegional.Types.IPSetDescriptor

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Specifies the type of update to perform to an IPSet with UpdateIPSet.
--
-- /See:/ 'newIPSetUpdate' smart constructor.
data IPSetUpdate = IPSetUpdate'
  { -- | Specifies whether to insert or delete an IP address with UpdateIPSet.
    action :: ChangeAction,
    -- | The IP address type (@IPV4@ or @IPV6@) and the IP address range (in CIDR
    -- notation) that web requests originate from.
    iPSetDescriptor :: IPSetDescriptor
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IPSetUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'iPSetUpdate_action' - Specifies whether to insert or delete an IP address with UpdateIPSet.
--
-- 'iPSetDescriptor', 'iPSetUpdate_iPSetDescriptor' - The IP address type (@IPV4@ or @IPV6@) and the IP address range (in CIDR
-- notation) that web requests originate from.
newIPSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'iPSetDescriptor'
  IPSetDescriptor ->
  IPSetUpdate
newIPSetUpdate pAction_ pIPSetDescriptor_ =
  IPSetUpdate'
    { action = pAction_,
      iPSetDescriptor = pIPSetDescriptor_
    }

-- | Specifies whether to insert or delete an IP address with UpdateIPSet.
iPSetUpdate_action :: Lens.Lens' IPSetUpdate ChangeAction
iPSetUpdate_action = Lens.lens (\IPSetUpdate' {action} -> action) (\s@IPSetUpdate' {} a -> s {action = a} :: IPSetUpdate)

-- | The IP address type (@IPV4@ or @IPV6@) and the IP address range (in CIDR
-- notation) that web requests originate from.
iPSetUpdate_iPSetDescriptor :: Lens.Lens' IPSetUpdate IPSetDescriptor
iPSetUpdate_iPSetDescriptor = Lens.lens (\IPSetUpdate' {iPSetDescriptor} -> iPSetDescriptor) (\s@IPSetUpdate' {} a -> s {iPSetDescriptor = a} :: IPSetUpdate)

instance Prelude.Hashable IPSetUpdate where
  hashWithSalt _salt IPSetUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` iPSetDescriptor

instance Prelude.NFData IPSetUpdate where
  rnf IPSetUpdate' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf iPSetDescriptor

instance Data.ToJSON IPSetUpdate where
  toJSON IPSetUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Data..= action),
            Prelude.Just
              ("IPSetDescriptor" Data..= iPSetDescriptor)
          ]
      )
