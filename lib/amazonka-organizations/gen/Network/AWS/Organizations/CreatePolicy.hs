{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.CreatePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy of a specified type that you can attach to a root, an organizational unit (OU), or an individual AWS account.
--
-- For more information about policies and their use, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html Managing Organization Policies> .
-- If the request includes tags, then the requester must have the @organizations:TagResource@ permission.
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.CreatePolicy
  ( -- * Creating a request
    CreatePolicy (..),
    mkCreatePolicy,

    -- ** Request lenses
    cpContent,
    cpName,
    cpType,
    cpDescription,
    cpTags,

    -- * Destructuring the response
    CreatePolicyResponse (..),
    mkCreatePolicyResponse,

    -- ** Response lenses
    cprsPolicy,
    cprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { -- | The policy text content to add to the new policy. The text that you supply must adhere to the rules of the policy type you specify in the @Type@ parameter.
    content :: Lude.Text,
    -- | The friendly name to assign to the policy.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
    name :: Lude.Text,
    -- | The type of policy to create. You can specify one of the following values:
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
    type' :: PolicyType,
    -- | An optional description to assign to the policy.
    description :: Lude.Text,
    -- | A list of tags that you want to attach to the newly created policy. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePolicy' with the minimum fields required to make a request.
--
-- * 'content' - The policy text content to add to the new policy. The text that you supply must adhere to the rules of the policy type you specify in the @Type@ parameter.
-- * 'name' - The friendly name to assign to the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
-- * 'type'' - The type of policy to create. You can specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
--
--
-- * 'description' - An optional description to assign to the policy.
-- * 'tags' - A list of tags that you want to attach to the newly created policy. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
mkCreatePolicy ::
  -- | 'content'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  PolicyType ->
  -- | 'description'
  Lude.Text ->
  CreatePolicy
mkCreatePolicy pContent_ pName_ pType_ pDescription_ =
  CreatePolicy'
    { content = pContent_,
      name = pName_,
      type' = pType_,
      description = pDescription_,
      tags = Lude.Nothing
    }

-- | The policy text content to add to the new policy. The text that you supply must adhere to the rules of the policy type you specify in the @Type@ parameter.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpContent :: Lens.Lens' CreatePolicy Lude.Text
cpContent = Lens.lens (content :: CreatePolicy -> Lude.Text) (\s a -> s {content = a} :: CreatePolicy)
{-# DEPRECATED cpContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The friendly name to assign to the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreatePolicy Lude.Text
cpName = Lens.lens (name :: CreatePolicy -> Lude.Text) (\s a -> s {name = a} :: CreatePolicy)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of policy to create. You can specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpType :: Lens.Lens' CreatePolicy PolicyType
cpType = Lens.lens (type' :: CreatePolicy -> PolicyType) (\s a -> s {type' = a} :: CreatePolicy)
{-# DEPRECATED cpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | An optional description to assign to the policy.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreatePolicy Lude.Text
cpDescription = Lens.lens (description :: CreatePolicy -> Lude.Text) (\s a -> s {description = a} :: CreatePolicy)
{-# DEPRECATED cpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags that you want to attach to the newly created policy. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePolicy (Lude.Maybe [Tag])
cpTags = Lens.lens (tags :: CreatePolicy -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePolicy)
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreatePolicy where
  type Rs CreatePolicy = CreatePolicyResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePolicyResponse'
            Lude.<$> (x Lude..?> "Policy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.CreatePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePolicy where
  toJSON CreatePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Content" Lude..= content),
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Type" Lude..= type'),
            Lude.Just ("Description" Lude..= description),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreatePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { -- | A structure that contains details about the newly created policy.
    policy :: Lude.Maybe Policy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - A structure that contains details about the newly created policy.
-- * 'responseStatus' - The response status code.
mkCreatePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePolicyResponse
mkCreatePolicyResponse pResponseStatus_ =
  CreatePolicyResponse'
    { policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the newly created policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsPolicy :: Lens.Lens' CreatePolicyResponse (Lude.Maybe Policy)
cprsPolicy = Lens.lens (policy :: CreatePolicyResponse -> Lude.Maybe Policy) (\s a -> s {policy = a} :: CreatePolicyResponse)
{-# DEPRECATED cprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreatePolicyResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreatePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePolicyResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
