{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.DeleteConfigRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified AWS Config rule and all of its evaluation results.
--
-- AWS Config sets the state of a rule to @DELETING@ until the deletion is
-- complete. You cannot update a rule while it is in this state. If you
-- make a @PutConfigRule@ or @DeleteConfigRule@ request for the rule, you
-- will receive a @ResourceInUseException@.
--
-- You can check the state of a rule by using the @DescribeConfigRules@
-- request.
module Network.AWS.Config.DeleteConfigRule
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

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteConfigRule' smart constructor.
data DeleteConfigRule = DeleteConfigRule'
  { -- | The name of the AWS Config rule that you want to delete.
    configRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleName', 'deleteConfigRule_configRuleName' - The name of the AWS Config rule that you want to delete.
newDeleteConfigRule ::
  -- | 'configRuleName'
  Prelude.Text ->
  DeleteConfigRule
newDeleteConfigRule pConfigRuleName_ =
  DeleteConfigRule'
    { configRuleName =
        pConfigRuleName_
    }

-- | The name of the AWS Config rule that you want to delete.
deleteConfigRule_configRuleName :: Lens.Lens' DeleteConfigRule Prelude.Text
deleteConfigRule_configRuleName = Lens.lens (\DeleteConfigRule' {configRuleName} -> configRuleName) (\s@DeleteConfigRule' {} a -> s {configRuleName = a} :: DeleteConfigRule)

instance Prelude.AWSRequest DeleteConfigRule where
  type Rs DeleteConfigRule = DeleteConfigRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteConfigRuleResponse'

instance Prelude.Hashable DeleteConfigRule

instance Prelude.NFData DeleteConfigRule

instance Prelude.ToHeaders DeleteConfigRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DeleteConfigRule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteConfigRule where
  toJSON DeleteConfigRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConfigRuleName" Prelude..= configRuleName)
          ]
      )

instance Prelude.ToPath DeleteConfigRule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteConfigRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConfigRuleResponse' smart constructor.
data DeleteConfigRuleResponse = DeleteConfigRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConfigRuleResponse ::
  DeleteConfigRuleResponse
newDeleteConfigRuleResponse =
  DeleteConfigRuleResponse'

instance Prelude.NFData DeleteConfigRuleResponse
