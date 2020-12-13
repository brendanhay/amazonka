{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.CreateOrganizationalUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an organizational unit (OU) within a root or parent OU. An OU is a container for accounts that enables you to organize your accounts to apply policies according to your business requirements. The number of levels deep that you can nest OUs is dependent upon the policy types enabled for that root. For service control policies, the limit is five.
--
-- For more information about OUs, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_ous.html Managing Organizational Units> in the /AWS Organizations User Guide./
-- If the request includes tags, then the requester must have the @organizations:TagResource@ permission.
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.CreateOrganizationalUnit
  ( -- * Creating a request
    CreateOrganizationalUnit (..),
    mkCreateOrganizationalUnit,

    -- ** Request lenses
    couName,
    couTags,
    couParentId,

    -- * Destructuring the response
    CreateOrganizationalUnitResponse (..),
    mkCreateOrganizationalUnitResponse,

    -- ** Response lenses
    coursOrganizationalUnit,
    coursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateOrganizationalUnit' smart constructor.
data CreateOrganizationalUnit = CreateOrganizationalUnit'
  { -- | The friendly name to assign to the new OU.
    name :: Lude.Text,
    -- | A list of tags that you want to attach to the newly created OU. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
    tags :: Lude.Maybe [Tag],
    -- | The unique identifier (ID) of the parent root or OU that you want to create the new OU in.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
    --
    --     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
    --
    --
    --     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
    parentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOrganizationalUnit' with the minimum fields required to make a request.
--
-- * 'name' - The friendly name to assign to the new OU.
-- * 'tags' - A list of tags that you want to attach to the newly created OU. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
-- * 'parentId' - The unique identifier (ID) of the parent root or OU that you want to create the new OU in.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
mkCreateOrganizationalUnit ::
  -- | 'name'
  Lude.Text ->
  -- | 'parentId'
  Lude.Text ->
  CreateOrganizationalUnit
mkCreateOrganizationalUnit pName_ pParentId_ =
  CreateOrganizationalUnit'
    { name = pName_,
      tags = Lude.Nothing,
      parentId = pParentId_
    }

-- | The friendly name to assign to the new OU.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
couName :: Lens.Lens' CreateOrganizationalUnit Lude.Text
couName = Lens.lens (name :: CreateOrganizationalUnit -> Lude.Text) (\s a -> s {name = a} :: CreateOrganizationalUnit)
{-# DEPRECATED couName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of tags that you want to attach to the newly created OU. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
couTags :: Lens.Lens' CreateOrganizationalUnit (Lude.Maybe [Tag])
couTags = Lens.lens (tags :: CreateOrganizationalUnit -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateOrganizationalUnit)
{-# DEPRECATED couTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The unique identifier (ID) of the parent root or OU that you want to create the new OU in.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
couParentId :: Lens.Lens' CreateOrganizationalUnit Lude.Text
couParentId = Lens.lens (parentId :: CreateOrganizationalUnit -> Lude.Text) (\s a -> s {parentId = a} :: CreateOrganizationalUnit)
{-# DEPRECATED couParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

instance Lude.AWSRequest CreateOrganizationalUnit where
  type Rs CreateOrganizationalUnit = CreateOrganizationalUnitResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateOrganizationalUnitResponse'
            Lude.<$> (x Lude..?> "OrganizationalUnit")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateOrganizationalUnit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.CreateOrganizationalUnit" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateOrganizationalUnit where
  toJSON CreateOrganizationalUnit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ParentId" Lude..= parentId)
          ]
      )

instance Lude.ToPath CreateOrganizationalUnit where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateOrganizationalUnit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateOrganizationalUnitResponse' smart constructor.
data CreateOrganizationalUnitResponse = CreateOrganizationalUnitResponse'
  { -- | A structure that contains details about the newly created OU.
    organizationalUnit :: Lude.Maybe OrganizationalUnit,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOrganizationalUnitResponse' with the minimum fields required to make a request.
--
-- * 'organizationalUnit' - A structure that contains details about the newly created OU.
-- * 'responseStatus' - The response status code.
mkCreateOrganizationalUnitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateOrganizationalUnitResponse
mkCreateOrganizationalUnitResponse pResponseStatus_ =
  CreateOrganizationalUnitResponse'
    { organizationalUnit =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the newly created OU.
--
-- /Note:/ Consider using 'organizationalUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coursOrganizationalUnit :: Lens.Lens' CreateOrganizationalUnitResponse (Lude.Maybe OrganizationalUnit)
coursOrganizationalUnit = Lens.lens (organizationalUnit :: CreateOrganizationalUnitResponse -> Lude.Maybe OrganizationalUnit) (\s a -> s {organizationalUnit = a} :: CreateOrganizationalUnitResponse)
{-# DEPRECATED coursOrganizationalUnit "Use generic-lens or generic-optics with 'organizationalUnit' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coursResponseStatus :: Lens.Lens' CreateOrganizationalUnitResponse Lude.Int
coursResponseStatus = Lens.lens (responseStatus :: CreateOrganizationalUnitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateOrganizationalUnitResponse)
{-# DEPRECATED coursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
