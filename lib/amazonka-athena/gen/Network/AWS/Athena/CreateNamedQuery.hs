{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.CreateNamedQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a named query in the specified workgroup. Requires that you have access to the workgroup.
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
module Network.AWS.Athena.CreateNamedQuery
  ( -- * Creating a request
    CreateNamedQuery (..),
    mkCreateNamedQuery,

    -- ** Request lenses
    cnqClientRequestToken,
    cnqDescription,
    cnqWorkGroup,
    cnqName,
    cnqDatabase,
    cnqQueryString,

    -- * Destructuring the response
    CreateNamedQueryResponse (..),
    mkCreateNamedQueryResponse,

    -- ** Response lenses
    cnqrsNamedQueryId,
    cnqrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateNamedQuery' smart constructor.
data CreateNamedQuery = CreateNamedQuery'
  { clientRequestToken ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    workGroup :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    database :: Lude.Text,
    queryString :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNamedQuery' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A unique case-sensitive string used to ensure the request to create the query is idempotent (executes only once). If another @CreateNamedQuery@ request is received, the same response is returned and another query is not created. If a parameter has changed, for example, the @QueryString@ , an error is returned.
--
-- /Important:/ This token is listed as not required because AWS SDKs (for example the AWS SDK for Java) auto-generate the token for users. If you are not using the AWS SDK or the AWS CLI, you must provide this token or the action will fail.
-- * 'database' - The database to which the query belongs.
-- * 'description' - The query description.
-- * 'name' - The query name.
-- * 'queryString' - The contents of the query with all query statements.
-- * 'workGroup' - The name of the workgroup in which the named query is being created.
mkCreateNamedQuery ::
  -- | 'name'
  Lude.Text ->
  -- | 'database'
  Lude.Text ->
  -- | 'queryString'
  Lude.Text ->
  CreateNamedQuery
mkCreateNamedQuery pName_ pDatabase_ pQueryString_ =
  CreateNamedQuery'
    { clientRequestToken = Lude.Nothing,
      description = Lude.Nothing,
      workGroup = Lude.Nothing,
      name = pName_,
      database = pDatabase_,
      queryString = pQueryString_
    }

-- | A unique case-sensitive string used to ensure the request to create the query is idempotent (executes only once). If another @CreateNamedQuery@ request is received, the same response is returned and another query is not created. If a parameter has changed, for example, the @QueryString@ , an error is returned.
--
-- /Important:/ This token is listed as not required because AWS SDKs (for example the AWS SDK for Java) auto-generate the token for users. If you are not using the AWS SDK or the AWS CLI, you must provide this token or the action will fail.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqClientRequestToken :: Lens.Lens' CreateNamedQuery (Lude.Maybe Lude.Text)
cnqClientRequestToken = Lens.lens (clientRequestToken :: CreateNamedQuery -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateNamedQuery)
{-# DEPRECATED cnqClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The query description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqDescription :: Lens.Lens' CreateNamedQuery (Lude.Maybe Lude.Text)
cnqDescription = Lens.lens (description :: CreateNamedQuery -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateNamedQuery)
{-# DEPRECATED cnqDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the workgroup in which the named query is being created.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqWorkGroup :: Lens.Lens' CreateNamedQuery (Lude.Maybe Lude.Text)
cnqWorkGroup = Lens.lens (workGroup :: CreateNamedQuery -> Lude.Maybe Lude.Text) (\s a -> s {workGroup = a} :: CreateNamedQuery)
{-# DEPRECATED cnqWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

-- | The query name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqName :: Lens.Lens' CreateNamedQuery Lude.Text
cnqName = Lens.lens (name :: CreateNamedQuery -> Lude.Text) (\s a -> s {name = a} :: CreateNamedQuery)
{-# DEPRECATED cnqName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The database to which the query belongs.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqDatabase :: Lens.Lens' CreateNamedQuery Lude.Text
cnqDatabase = Lens.lens (database :: CreateNamedQuery -> Lude.Text) (\s a -> s {database = a} :: CreateNamedQuery)
{-# DEPRECATED cnqDatabase "Use generic-lens or generic-optics with 'database' instead." #-}

-- | The contents of the query with all query statements.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqQueryString :: Lens.Lens' CreateNamedQuery Lude.Text
cnqQueryString = Lens.lens (queryString :: CreateNamedQuery -> Lude.Text) (\s a -> s {queryString = a} :: CreateNamedQuery)
{-# DEPRECATED cnqQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

instance Lude.AWSRequest CreateNamedQuery where
  type Rs CreateNamedQuery = CreateNamedQueryResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateNamedQueryResponse'
            Lude.<$> (x Lude..?> "NamedQueryId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateNamedQuery where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.CreateNamedQuery" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateNamedQuery where
  toJSON CreateNamedQuery' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("Description" Lude..=) Lude.<$> description,
            ("WorkGroup" Lude..=) Lude.<$> workGroup,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Database" Lude..= database),
            Lude.Just ("QueryString" Lude..= queryString)
          ]
      )

instance Lude.ToPath CreateNamedQuery where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateNamedQuery where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateNamedQueryResponse' smart constructor.
data CreateNamedQueryResponse = CreateNamedQueryResponse'
  { namedQueryId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateNamedQueryResponse' with the minimum fields required to make a request.
--
-- * 'namedQueryId' - The unique ID of the query.
-- * 'responseStatus' - The response status code.
mkCreateNamedQueryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateNamedQueryResponse
mkCreateNamedQueryResponse pResponseStatus_ =
  CreateNamedQueryResponse'
    { namedQueryId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID of the query.
--
-- /Note:/ Consider using 'namedQueryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqrsNamedQueryId :: Lens.Lens' CreateNamedQueryResponse (Lude.Maybe Lude.Text)
cnqrsNamedQueryId = Lens.lens (namedQueryId :: CreateNamedQueryResponse -> Lude.Maybe Lude.Text) (\s a -> s {namedQueryId = a} :: CreateNamedQueryResponse)
{-# DEPRECATED cnqrsNamedQueryId "Use generic-lens or generic-optics with 'namedQueryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnqrsResponseStatus :: Lens.Lens' CreateNamedQueryResponse Lude.Int
cnqrsResponseStatus = Lens.lens (responseStatus :: CreateNamedQueryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateNamedQueryResponse)
{-# DEPRECATED cnqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
