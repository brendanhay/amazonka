{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to a specified resource.
--
-- A resource can have up to 10 tags. If you try to create more than 10
-- tags for a resource, you will receive an error and the attempt will
-- fail.
--
-- If you specify a key that already exists for the resource, the value for
-- that key will be updated with the new value.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateTags.html AWS API Reference> for CreateTags.
module Network.AWS.Redshift.CreateTags
    (
    -- * Creating a Request
      CreateTags
    , createTags
    -- * Request Lenses
    , ctResourceName
    , ctTags

    -- * Destructuring the Response
    , CreateTagsResponse
    , createTagsResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the output from the @CreateTags@ action.
--
-- /See:/ 'createTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctResourceName'
--
-- * 'ctTags'
data CreateTags = CreateTags'
    { _ctResourceName :: !Text
    , _ctTags         :: ![Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateTags' smart constructor.
createTags :: Text -> CreateTags
createTags pResourceName_ =
    CreateTags'
    { _ctResourceName = pResourceName_
    , _ctTags = mempty
    }

-- | The Amazon Resource Name (ARN) to which you want to add the tag or tags.
-- For example, @arn:aws:redshift:us-east-1:123456789:cluster:t1@.
ctResourceName :: Lens' CreateTags Text
ctResourceName = lens _ctResourceName (\ s a -> s{_ctResourceName = a});

-- | One or more name\/value pairs to add as tags to the specified resource.
-- Each tag name is passed in with the parameter @Key@ and the
-- corresponding value is passed in with the parameter @Value@. The @Key@
-- and @Value@ parameters are separated by a comma (,). Separate multiple
-- tags with a space. For example,
-- @--tags \"Key\"=\"owner\",\"Value\"=\"admin\" \"Key\"=\"environment\",\"Value\"=\"test\" \"Key\"=\"version\",\"Value\"=\"1.0\"@.
ctTags :: Lens' CreateTags [Tag]
ctTags = lens _ctTags (\ s a -> s{_ctTags = a}) . _Coerce;

instance AWSRequest CreateTags where
        type Sv CreateTags = Redshift
        type Rs CreateTags = CreateTagsResponse
        request = postQuery
        response = receiveNull CreateTagsResponse'

instance ToHeaders CreateTags where
        toHeaders = const mempty

instance ToPath CreateTags where
        toPath = const "/"

instance ToQuery CreateTags where
        toQuery CreateTags'{..}
          = mconcat
              ["Action" =: ("CreateTags" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ResourceName" =: _ctResourceName,
               "Tags" =: toQueryList "Tag" _ctTags]

-- | /See:/ 'createTagsResponse' smart constructor.
data CreateTagsResponse =
    CreateTagsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateTagsResponse' smart constructor.
createTagsResponse :: CreateTagsResponse
createTagsResponse = CreateTagsResponse'
