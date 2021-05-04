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
-- Module      : Network.AWS.RDS.DeleteOptionGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing option group.
module Network.AWS.RDS.DeleteOptionGroup
  ( -- * Creating a Request
    DeleteOptionGroup (..),
    newDeleteOptionGroup,

    -- * Request Lenses
    deleteOptionGroup_optionGroupName,

    -- * Destructuring the Response
    DeleteOptionGroupResponse (..),
    newDeleteOptionGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteOptionGroup' smart constructor.
data DeleteOptionGroup = DeleteOptionGroup'
  { -- | The name of the option group to be deleted.
    --
    -- You can\'t delete default option groups.
    optionGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteOptionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionGroupName', 'deleteOptionGroup_optionGroupName' - The name of the option group to be deleted.
--
-- You can\'t delete default option groups.
newDeleteOptionGroup ::
  -- | 'optionGroupName'
  Prelude.Text ->
  DeleteOptionGroup
newDeleteOptionGroup pOptionGroupName_ =
  DeleteOptionGroup'
    { optionGroupName =
        pOptionGroupName_
    }

-- | The name of the option group to be deleted.
--
-- You can\'t delete default option groups.
deleteOptionGroup_optionGroupName :: Lens.Lens' DeleteOptionGroup Prelude.Text
deleteOptionGroup_optionGroupName = Lens.lens (\DeleteOptionGroup' {optionGroupName} -> optionGroupName) (\s@DeleteOptionGroup' {} a -> s {optionGroupName = a} :: DeleteOptionGroup)

instance Prelude.AWSRequest DeleteOptionGroup where
  type Rs DeleteOptionGroup = DeleteOptionGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteOptionGroupResponse'

instance Prelude.Hashable DeleteOptionGroup

instance Prelude.NFData DeleteOptionGroup

instance Prelude.ToHeaders DeleteOptionGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteOptionGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteOptionGroup where
  toQuery DeleteOptionGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteOptionGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2014-10-31" :: Prelude.ByteString),
        "OptionGroupName" Prelude.=: optionGroupName
      ]

-- | /See:/ 'newDeleteOptionGroupResponse' smart constructor.
data DeleteOptionGroupResponse = DeleteOptionGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteOptionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteOptionGroupResponse ::
  DeleteOptionGroupResponse
newDeleteOptionGroupResponse =
  DeleteOptionGroupResponse'

instance Prelude.NFData DeleteOptionGroupResponse
