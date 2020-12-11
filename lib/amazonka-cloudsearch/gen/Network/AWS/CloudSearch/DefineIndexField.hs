{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineIndexField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an @'IndexField' @ for the search domain. Used to create new fields and modify existing ones. You must specify the name of the domain you are configuring and an index field configuration. The index field configuration specifies a unique name, the index field type, and the options you want to configure for the field. The options you can specify depend on the @'IndexFieldType' @ . If the field exists, the new configuration replaces the old one. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DefineIndexField
  ( -- * Creating a request
    DefineIndexField (..),
    mkDefineIndexField,

    -- ** Request lenses
    defeDomainName,
    defeIndexField,

    -- * Destructuring the response
    DefineIndexFieldResponse (..),
    mkDefineIndexFieldResponse,

    -- ** Response lenses
    defrsResponseStatus,
    defrsIndexField,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DefineIndexField' @ operation. Specifies the name of the domain you want to update and the index field configuration.
--
-- /See:/ 'mkDefineIndexField' smart constructor.
data DefineIndexField = DefineIndexField'
  { domainName :: Lude.Text,
    indexField :: IndexField
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefineIndexField' with the minimum fields required to make a request.
--
-- * 'domainName' - Undocumented field.
-- * 'indexField' - The index field and field options you want to configure.
mkDefineIndexField ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'indexField'
  IndexField ->
  DefineIndexField
mkDefineIndexField pDomainName_ pIndexField_ =
  DefineIndexField'
    { domainName = pDomainName_,
      indexField = pIndexField_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defeDomainName :: Lens.Lens' DefineIndexField Lude.Text
defeDomainName = Lens.lens (domainName :: DefineIndexField -> Lude.Text) (\s a -> s {domainName = a} :: DefineIndexField)
{-# DEPRECATED defeDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The index field and field options you want to configure.
--
-- /Note:/ Consider using 'indexField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defeIndexField :: Lens.Lens' DefineIndexField IndexField
defeIndexField = Lens.lens (indexField :: DefineIndexField -> IndexField) (\s a -> s {indexField = a} :: DefineIndexField)
{-# DEPRECATED defeIndexField "Use generic-lens or generic-optics with 'indexField' instead." #-}

instance Lude.AWSRequest DefineIndexField where
  type Rs DefineIndexField = DefineIndexFieldResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DefineIndexFieldResult"
      ( \s h x ->
          DefineIndexFieldResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "IndexField")
      )

instance Lude.ToHeaders DefineIndexField where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DefineIndexField where
  toPath = Lude.const "/"

instance Lude.ToQuery DefineIndexField where
  toQuery DefineIndexField' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DefineIndexField" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName,
        "IndexField" Lude.=: indexField
      ]

-- | The result of a @'DefineIndexField' @ request. Contains the status of the newly-configured index field.
--
-- /See:/ 'mkDefineIndexFieldResponse' smart constructor.
data DefineIndexFieldResponse = DefineIndexFieldResponse'
  { responseStatus ::
      Lude.Int,
    indexField :: IndexFieldStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefineIndexFieldResponse' with the minimum fields required to make a request.
--
-- * 'indexField' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDefineIndexFieldResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'indexField'
  IndexFieldStatus ->
  DefineIndexFieldResponse
mkDefineIndexFieldResponse pResponseStatus_ pIndexField_ =
  DefineIndexFieldResponse'
    { responseStatus = pResponseStatus_,
      indexField = pIndexField_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defrsResponseStatus :: Lens.Lens' DefineIndexFieldResponse Lude.Int
defrsResponseStatus = Lens.lens (responseStatus :: DefineIndexFieldResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DefineIndexFieldResponse)
{-# DEPRECATED defrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'indexField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defrsIndexField :: Lens.Lens' DefineIndexFieldResponse IndexFieldStatus
defrsIndexField = Lens.lens (indexField :: DefineIndexFieldResponse -> IndexFieldStatus) (\s a -> s {indexField = a} :: DefineIndexFieldResponse)
{-# DEPRECATED defrsIndexField "Use generic-lens or generic-optics with 'indexField' instead." #-}
