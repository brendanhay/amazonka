{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeIndexFields
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the index fields configured for the search domain. Can be limited to specific fields by name. By default, shows all fields and includes any pending changes to the configuration. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-domain-info.html Getting Domain Information> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeIndexFields
  ( -- * Creating a request
    DescribeIndexFields (..),
    mkDescribeIndexFields,

    -- ** Request lenses
    difDeployed,
    difFieldNames,
    difDomainName,

    -- * Destructuring the response
    DescribeIndexFieldsResponse (..),
    mkDescribeIndexFieldsResponse,

    -- ** Response lenses
    difsrsResponseStatus,
    difsrsIndexFields,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DescribeIndexFields' @ operation. Specifies the name of the domain you want to describe. To restrict the response to particular index fields, specify the names of the index fields you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
-- /See:/ 'mkDescribeIndexFields' smart constructor.
data DescribeIndexFields = DescribeIndexFields'
  { deployed ::
      Lude.Maybe Lude.Bool,
    fieldNames :: Lude.Maybe [Lude.Text],
    domainName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIndexFields' with the minimum fields required to make a request.
--
-- * 'deployed' - Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
-- * 'domainName' - The name of the domain you want to describe.
-- * 'fieldNames' - A list of the index fields you want to describe. If not specified, information is returned for all configured index fields.
mkDescribeIndexFields ::
  -- | 'domainName'
  Lude.Text ->
  DescribeIndexFields
mkDescribeIndexFields pDomainName_ =
  DescribeIndexFields'
    { deployed = Lude.Nothing,
      fieldNames = Lude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difDeployed :: Lens.Lens' DescribeIndexFields (Lude.Maybe Lude.Bool)
difDeployed = Lens.lens (deployed :: DescribeIndexFields -> Lude.Maybe Lude.Bool) (\s a -> s {deployed = a} :: DescribeIndexFields)
{-# DEPRECATED difDeployed "Use generic-lens or generic-optics with 'deployed' instead." #-}

-- | A list of the index fields you want to describe. If not specified, information is returned for all configured index fields.
--
-- /Note:/ Consider using 'fieldNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difFieldNames :: Lens.Lens' DescribeIndexFields (Lude.Maybe [Lude.Text])
difFieldNames = Lens.lens (fieldNames :: DescribeIndexFields -> Lude.Maybe [Lude.Text]) (\s a -> s {fieldNames = a} :: DescribeIndexFields)
{-# DEPRECATED difFieldNames "Use generic-lens or generic-optics with 'fieldNames' instead." #-}

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difDomainName :: Lens.Lens' DescribeIndexFields Lude.Text
difDomainName = Lens.lens (domainName :: DescribeIndexFields -> Lude.Text) (\s a -> s {domainName = a} :: DescribeIndexFields)
{-# DEPRECATED difDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DescribeIndexFields where
  type Rs DescribeIndexFields = DescribeIndexFieldsResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DescribeIndexFieldsResult"
      ( \s h x ->
          DescribeIndexFieldsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "IndexFields" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders DescribeIndexFields where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeIndexFields where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeIndexFields where
  toQuery DescribeIndexFields' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeIndexFields" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "Deployed" Lude.=: deployed,
        "FieldNames"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> fieldNames),
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @DescribeIndexFields@ request. Contains the index fields configured for the domain specified in the request.
--
-- /See:/ 'mkDescribeIndexFieldsResponse' smart constructor.
data DescribeIndexFieldsResponse = DescribeIndexFieldsResponse'
  { responseStatus ::
      Lude.Int,
    indexFields :: [IndexFieldStatus]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIndexFieldsResponse' with the minimum fields required to make a request.
--
-- * 'indexFields' - The index fields configured for the domain.
-- * 'responseStatus' - The response status code.
mkDescribeIndexFieldsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeIndexFieldsResponse
mkDescribeIndexFieldsResponse pResponseStatus_ =
  DescribeIndexFieldsResponse'
    { responseStatus = pResponseStatus_,
      indexFields = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difsrsResponseStatus :: Lens.Lens' DescribeIndexFieldsResponse Lude.Int
difsrsResponseStatus = Lens.lens (responseStatus :: DescribeIndexFieldsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeIndexFieldsResponse)
{-# DEPRECATED difsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The index fields configured for the domain.
--
-- /Note:/ Consider using 'indexFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difsrsIndexFields :: Lens.Lens' DescribeIndexFieldsResponse [IndexFieldStatus]
difsrsIndexFields = Lens.lens (indexFields :: DescribeIndexFieldsResponse -> [IndexFieldStatus]) (\s a -> s {indexFields = a} :: DescribeIndexFieldsResponse)
{-# DEPRECATED difsrsIndexFields "Use generic-lens or generic-optics with 'indexFields' instead." #-}
