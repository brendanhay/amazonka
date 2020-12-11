{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.CreateParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new parameter group. A parameter group is a collection of parameters that you apply to all of the nodes in a DAX cluster.
module Network.AWS.DAX.CreateParameterGroup
  ( -- * Creating a request
    CreateParameterGroup (..),
    mkCreateParameterGroup,

    -- ** Request lenses
    cpgDescription,
    cpgParameterGroupName,

    -- * Destructuring the response
    CreateParameterGroupResponse (..),
    mkCreateParameterGroupResponse,

    -- ** Response lenses
    cpgrsParameterGroup,
    cpgrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateParameterGroup' smart constructor.
data CreateParameterGroup = CreateParameterGroup'
  { description ::
      Lude.Maybe Lude.Text,
    parameterGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateParameterGroup' with the minimum fields required to make a request.
--
-- * 'description' - A description of the parameter group.
-- * 'parameterGroupName' - The name of the parameter group to apply to all of the clusters in this replication group.
mkCreateParameterGroup ::
  -- | 'parameterGroupName'
  Lude.Text ->
  CreateParameterGroup
mkCreateParameterGroup pParameterGroupName_ =
  CreateParameterGroup'
    { description = Lude.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | A description of the parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgDescription :: Lens.Lens' CreateParameterGroup (Lude.Maybe Lude.Text)
cpgDescription = Lens.lens (description :: CreateParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateParameterGroup)
{-# DEPRECATED cpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the parameter group to apply to all of the clusters in this replication group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgParameterGroupName :: Lens.Lens' CreateParameterGroup Lude.Text
cpgParameterGroupName = Lens.lens (parameterGroupName :: CreateParameterGroup -> Lude.Text) (\s a -> s {parameterGroupName = a} :: CreateParameterGroup)
{-# DEPRECATED cpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Lude.AWSRequest CreateParameterGroup where
  type Rs CreateParameterGroup = CreateParameterGroupResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateParameterGroupResponse'
            Lude.<$> (x Lude..?> "ParameterGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateParameterGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.CreateParameterGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateParameterGroup where
  toJSON CreateParameterGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("ParameterGroupName" Lude..= parameterGroupName)
          ]
      )

instance Lude.ToPath CreateParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateParameterGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateParameterGroupResponse' smart constructor.
data CreateParameterGroupResponse = CreateParameterGroupResponse'
  { parameterGroup ::
      Lude.Maybe ParameterGroup,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateParameterGroupResponse' with the minimum fields required to make a request.
--
-- * 'parameterGroup' - Represents the output of a /CreateParameterGroup/ action.
-- * 'responseStatus' - The response status code.
mkCreateParameterGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateParameterGroupResponse
mkCreateParameterGroupResponse pResponseStatus_ =
  CreateParameterGroupResponse'
    { parameterGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the output of a /CreateParameterGroup/ action.
--
-- /Note:/ Consider using 'parameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgrsParameterGroup :: Lens.Lens' CreateParameterGroupResponse (Lude.Maybe ParameterGroup)
cpgrsParameterGroup = Lens.lens (parameterGroup :: CreateParameterGroupResponse -> Lude.Maybe ParameterGroup) (\s a -> s {parameterGroup = a} :: CreateParameterGroupResponse)
{-# DEPRECATED cpgrsParameterGroup "Use generic-lens or generic-optics with 'parameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgrsResponseStatus :: Lens.Lens' CreateParameterGroupResponse Lude.Int
cpgrsResponseStatus = Lens.lens (responseStatus :: CreateParameterGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateParameterGroupResponse)
{-# DEPRECATED cpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
