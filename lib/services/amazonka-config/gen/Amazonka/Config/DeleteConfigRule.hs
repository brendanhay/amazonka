{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Config.DeleteConfigRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Config rule and all of its evaluation results.
--
-- Config sets the state of a rule to @DELETING@ until the deletion is
-- complete. You cannot update a rule while it is in this state. If you
-- make a @PutConfigRule@ or @DeleteConfigRule@ request for the rule, you
-- will receive a @ResourceInUseException@.
--
-- You can check the state of a rule by using the @DescribeConfigRules@
-- request.
module Amazonka.Config.DeleteConfigRule
  ( -- * Creating a Request
    DeleteConfigRule (..),
    newDeleteConfigRule,

    -- * Request Lenses
    deleteConfigRule_configRuleName,

    -- * Destructuring the Response
    DeleteConfigRuleResponse (..),
    newDeleteConfigRuleResponse,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteConfigRule' smart constructor.
data DeleteConfigRule = DeleteConfigRule'
  { -- | The name of the Config rule that you want to delete.
    configRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleName', 'deleteConfigRule_configRuleName' - The name of the Config rule that you want to delete.
newDeleteConfigRule ::
  -- | 'configRuleName'
  Prelude.Text ->
  DeleteConfigRule
newDeleteConfigRule pConfigRuleName_ =
  DeleteConfigRule'
    { configRuleName =
        pConfigRuleName_
    }

-- | The name of the Config rule that you want to delete.
deleteConfigRule_configRuleName :: Lens.Lens' DeleteConfigRule Prelude.Text
deleteConfigRule_configRuleName = Lens.lens (\DeleteConfigRule' {configRuleName} -> configRuleName) (\s@DeleteConfigRule' {} a -> s {configRuleName = a} :: DeleteConfigRule)

instance Core.AWSRequest DeleteConfigRule where
  type
    AWSResponse DeleteConfigRule =
      DeleteConfigRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteConfigRuleResponse'

instance Prelude.Hashable DeleteConfigRule where
  hashWithSalt _salt DeleteConfigRule' {..} =
    _salt `Prelude.hashWithSalt` configRuleName

instance Prelude.NFData DeleteConfigRule where
  rnf DeleteConfigRule' {..} =
    Prelude.rnf configRuleName

instance Data.ToHeaders DeleteConfigRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DeleteConfigRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConfigRule where
  toJSON DeleteConfigRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConfigRuleName" Data..= configRuleName)
          ]
      )

instance Data.ToPath DeleteConfigRule where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConfigRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConfigRuleResponse' smart constructor.
data DeleteConfigRuleResponse = DeleteConfigRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConfigRuleResponse ::
  DeleteConfigRuleResponse
newDeleteConfigRuleResponse =
  DeleteConfigRuleResponse'

instance Prelude.NFData DeleteConfigRuleResponse where
  rnf _ = ()
