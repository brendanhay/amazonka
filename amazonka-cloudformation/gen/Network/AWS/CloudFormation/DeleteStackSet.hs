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
-- Module      : Network.AWS.CloudFormation.DeleteStackSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a stack set. Before you can delete a stack set, all of its
-- member stack instances must be deleted. For more information about how
-- to do this, see DeleteStackInstances.
module Network.AWS.CloudFormation.DeleteStackSet
  ( -- * Creating a Request
    DeleteStackSet (..),
    newDeleteStackSet,

    -- * Request Lenses
    deleteStackSet_callAs,
    deleteStackSet_stackSetName,

    -- * Destructuring the Response
    DeleteStackSetResponse (..),
    newDeleteStackSetResponse,

    -- * Response Lenses
    deleteStackSetResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteStackSet' smart constructor.
data DeleteStackSet = DeleteStackSet'
  { -- | [Service-managed permissions] Specifies whether you are acting as an
    -- account administrator in the organization\'s management account or as a
    -- delegated administrator in a member account.
    --
    -- By default, @SELF@ is specified. Use @SELF@ for stack sets with
    -- self-managed permissions.
    --
    -- -   If you are signed in to the management account, specify @SELF@.
    --
    -- -   If you are signed in to a delegated administrator account, specify
    --     @DELEGATED_ADMIN@.
    --
    --     Your AWS account must be registered as a delegated administrator in
    --     the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /AWS CloudFormation User Guide/.
    callAs :: Prelude.Maybe CallAs,
    -- | The name or unique ID of the stack set that you\'re deleting. You can
    -- obtain this value by running ListStackSets.
    stackSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteStackSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAs', 'deleteStackSet_callAs' - [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your AWS account must be registered as a delegated administrator in
--     the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
--
-- 'stackSetName', 'deleteStackSet_stackSetName' - The name or unique ID of the stack set that you\'re deleting. You can
-- obtain this value by running ListStackSets.
newDeleteStackSet ::
  -- | 'stackSetName'
  Prelude.Text ->
  DeleteStackSet
newDeleteStackSet pStackSetName_ =
  DeleteStackSet'
    { callAs = Prelude.Nothing,
      stackSetName = pStackSetName_
    }

-- | [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your AWS account must be registered as a delegated administrator in
--     the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
deleteStackSet_callAs :: Lens.Lens' DeleteStackSet (Prelude.Maybe CallAs)
deleteStackSet_callAs = Lens.lens (\DeleteStackSet' {callAs} -> callAs) (\s@DeleteStackSet' {} a -> s {callAs = a} :: DeleteStackSet)

-- | The name or unique ID of the stack set that you\'re deleting. You can
-- obtain this value by running ListStackSets.
deleteStackSet_stackSetName :: Lens.Lens' DeleteStackSet Prelude.Text
deleteStackSet_stackSetName = Lens.lens (\DeleteStackSet' {stackSetName} -> stackSetName) (\s@DeleteStackSet' {} a -> s {stackSetName = a} :: DeleteStackSet)

instance Prelude.AWSRequest DeleteStackSet where
  type Rs DeleteStackSet = DeleteStackSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteStackSetResult"
      ( \s h x ->
          DeleteStackSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStackSet

instance Prelude.NFData DeleteStackSet

instance Prelude.ToHeaders DeleteStackSet where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteStackSet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteStackSet where
  toQuery DeleteStackSet' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteStackSet" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "CallAs" Prelude.=: callAs,
        "StackSetName" Prelude.=: stackSetName
      ]

-- | /See:/ 'newDeleteStackSetResponse' smart constructor.
data DeleteStackSetResponse = DeleteStackSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteStackSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStackSetResponse_httpStatus' - The response's http status code.
newDeleteStackSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStackSetResponse
newDeleteStackSetResponse pHttpStatus_ =
  DeleteStackSetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteStackSetResponse_httpStatus :: Lens.Lens' DeleteStackSetResponse Prelude.Int
deleteStackSetResponse_httpStatus = Lens.lens (\DeleteStackSetResponse' {httpStatus} -> httpStatus) (\s@DeleteStackSetResponse' {} a -> s {httpStatus = a} :: DeleteStackSetResponse)

instance Prelude.NFData DeleteStackSetResponse
