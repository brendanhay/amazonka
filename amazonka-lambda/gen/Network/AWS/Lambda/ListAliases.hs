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
-- Module      : Network.AWS.Lambda.ListAliases
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of aliases created for a Lambda function. For each alias, the response includes information such as the alias ARN, description, alias name, and the function version to which it points. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html Introduction to AWS Lambda Aliases> .
--
--
-- This requires permission for the lambda:ListAliases action.
--
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListAliases
    (
    -- * Creating a Request
      listAliases
    , ListAliases
    -- * Request Lenses
    , laMarker
    , laMaxItems
    , laFunctionVersion
    , laFunctionName

    -- * Destructuring the Response
    , listAliasesResponse
    , ListAliasesResponse
    -- * Response Lenses
    , larsAliases
    , larsNextMarker
    , larsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAliases' smart constructor.
data ListAliases = ListAliases'
  { _laMarker          :: !(Maybe Text)
  , _laMaxItems        :: !(Maybe Nat)
  , _laFunctionVersion :: !(Maybe Text)
  , _laFunctionName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAliases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laMarker' - Optional string. An opaque pagination token returned from a previous @ListAliases@ operation. If present, indicates where to continue the listing.
--
-- * 'laMaxItems' - Optional integer. Specifies the maximum number of aliases to return in response. This parameter value must be greater than 0.
--
-- * 'laFunctionVersion' - If you specify this optional parameter, the API returns only the aliases that are pointing to the specific Lambda function version, otherwise the API returns all of the aliases created for the Lambda function.
--
-- * 'laFunctionName' - Lambda function name for which the alias is created. Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
listAliases
    :: Text -- ^ 'laFunctionName'
    -> ListAliases
listAliases pFunctionName_ =
  ListAliases'
    { _laMarker = Nothing
    , _laMaxItems = Nothing
    , _laFunctionVersion = Nothing
    , _laFunctionName = pFunctionName_
    }


-- | Optional string. An opaque pagination token returned from a previous @ListAliases@ operation. If present, indicates where to continue the listing.
laMarker :: Lens' ListAliases (Maybe Text)
laMarker = lens _laMarker (\ s a -> s{_laMarker = a})

-- | Optional integer. Specifies the maximum number of aliases to return in response. This parameter value must be greater than 0.
laMaxItems :: Lens' ListAliases (Maybe Natural)
laMaxItems = lens _laMaxItems (\ s a -> s{_laMaxItems = a}) . mapping _Nat

-- | If you specify this optional parameter, the API returns only the aliases that are pointing to the specific Lambda function version, otherwise the API returns all of the aliases created for the Lambda function.
laFunctionVersion :: Lens' ListAliases (Maybe Text)
laFunctionVersion = lens _laFunctionVersion (\ s a -> s{_laFunctionVersion = a})

-- | Lambda function name for which the alias is created. Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
laFunctionName :: Lens' ListAliases Text
laFunctionName = lens _laFunctionName (\ s a -> s{_laFunctionName = a})

instance AWSPager ListAliases where
        page rq rs
          | stop (rs ^. larsNextMarker) = Nothing
          | stop (rs ^. larsAliases) = Nothing
          | otherwise =
            Just $ rq & laMarker .~ rs ^. larsNextMarker

instance AWSRequest ListAliases where
        type Rs ListAliases = ListAliasesResponse
        request = get lambda
        response
          = receiveJSON
              (\ s h x ->
                 ListAliasesResponse' <$>
                   (x .?> "Aliases" .!@ mempty) <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListAliases where

instance NFData ListAliases where

instance ToHeaders ListAliases where
        toHeaders = const mempty

instance ToPath ListAliases where
        toPath ListAliases'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _laFunctionName,
               "/aliases"]

instance ToQuery ListAliases where
        toQuery ListAliases'{..}
          = mconcat
              ["Marker" =: _laMarker, "MaxItems" =: _laMaxItems,
               "FunctionVersion" =: _laFunctionVersion]

-- | /See:/ 'listAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { _larsAliases        :: !(Maybe [AliasConfiguration])
  , _larsNextMarker     :: !(Maybe Text)
  , _larsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAliasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsAliases' - A list of aliases.
--
-- * 'larsNextMarker' - A string, present if there are more aliases.
--
-- * 'larsResponseStatus' - -- | The response status code.
listAliasesResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListAliasesResponse
listAliasesResponse pResponseStatus_ =
  ListAliasesResponse'
    { _larsAliases = Nothing
    , _larsNextMarker = Nothing
    , _larsResponseStatus = pResponseStatus_
    }


-- | A list of aliases.
larsAliases :: Lens' ListAliasesResponse [AliasConfiguration]
larsAliases = lens _larsAliases (\ s a -> s{_larsAliases = a}) . _Default . _Coerce

-- | A string, present if there are more aliases.
larsNextMarker :: Lens' ListAliasesResponse (Maybe Text)
larsNextMarker = lens _larsNextMarker (\ s a -> s{_larsNextMarker = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListAliasesResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

instance NFData ListAliasesResponse where
