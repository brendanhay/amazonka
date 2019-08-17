{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lambda function <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> .
--
--
module Network.AWS.Lambda.DeleteAlias
    (
    -- * Creating a Request
      deleteAlias
    , DeleteAlias
    -- * Request Lenses
    , daFunctionName
    , daName

    -- * Destructuring the Response
    , deleteAliasResponse
    , DeleteAliasResponse
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { _daFunctionName :: !Text
  , _daName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'daName' - The name of the alias.
deleteAlias
    :: Text -- ^ 'daFunctionName'
    -> Text -- ^ 'daName'
    -> DeleteAlias
deleteAlias pFunctionName_ pName_ =
  DeleteAlias' {_daFunctionName = pFunctionName_, _daName = pName_}


-- | The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
daFunctionName :: Lens' DeleteAlias Text
daFunctionName = lens _daFunctionName (\ s a -> s{_daFunctionName = a})

-- | The name of the alias.
daName :: Lens' DeleteAlias Text
daName = lens _daName (\ s a -> s{_daName = a})

instance AWSRequest DeleteAlias where
        type Rs DeleteAlias = DeleteAliasResponse
        request = delete lambda
        response = receiveNull DeleteAliasResponse'

instance Hashable DeleteAlias where

instance NFData DeleteAlias where

instance ToHeaders DeleteAlias where
        toHeaders = const mempty

instance ToPath DeleteAlias where
        toPath DeleteAlias'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _daFunctionName,
               "/aliases/", toBS _daName]

instance ToQuery DeleteAlias where
        toQuery = const mempty

-- | /See:/ 'deleteAliasResponse' smart constructor.
data DeleteAliasResponse =
  DeleteAliasResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAliasResponse' with the minimum fields required to make a request.
--
deleteAliasResponse
    :: DeleteAliasResponse
deleteAliasResponse = DeleteAliasResponse'


instance NFData DeleteAliasResponse where
